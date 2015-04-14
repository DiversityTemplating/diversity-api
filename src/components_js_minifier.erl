-module(components_js_minifier).

-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

%% Cowboy callbacks

init(_, Req, _Opts) ->
    {ok, Req, []}.

handle(Req, State) ->
    {ok, Req1} = cowboy_req:chunked_reply(
        200, [{<<"content-type">>, <<"application/javascript">>}], Req
    ),
    %% Proplist with one or more components to minfify
    {Components, Req2} = cowboy_req:qs_vals(Req1),

    %% Build a map with all components like
    ComponentScriptFilePaths = pmap(
        fun ({Component, Tag}) ->
            {DiversityProps} = jiffy:decode(git_utils:get_diversity_json(Component, Tag)),
            ScriptFiles = proplists:get_value(<<"script">>, DiversityProps),
            TaggedScriptFiles = lists:map(fun get_file_type/1, ScriptFiles),
            {{Component, Tag}, TaggedScriptFiles}
        end,
        Components
    ),

    Z = zlib:open(),
    ok = zlib:deflateInit(Z, default),
    pmap(
        fun ({{Component, Tag}, FilePaths}) ->
            % zlib:deflateReset(Z),
            Result = get_minified_file(Component, Tag, FilePaths, fun get_file_contents/4),
            Result1 = zlib:deflate(Z, Result, finish),
            io:format("Result ~p" , [Result1]),
            cowboy_req:chunk(Result1, Req2),
            Result
        end,
        ComponentScriptFilePaths
    ),
    cowboy_req:chunk(zlib:deflate(Z, <<>>, finish), Req2),
    ok = zlib:deflateEnd(Z),
    zlib:close(Z),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.

get_file_type(<<"//", _Rest/binary>> = ScriptFile) ->
    %% This is an exteral js file that needs to be downloaded.
    {external, <<"http:", ScriptFile/binary>>};
get_file_type(<<"http://", _Rest/binary>> = ScriptFile) ->
    %% This is an exteral js file that needs to be downloaded.
    {external, ScriptFile};
get_file_type(<<"https://", _Rest/binary>> = ScriptFile) ->
    %% This is an exteral js file that needs to be downloaded.
    {external, ScriptFile};
get_file_type(ScriptFile) ->
    case re:run(ScriptFile, <<".min.js">>) of
        {match, _} ->
            %% It's already minified. And exists in our component already
            {already_minified, ScriptFile};
        _ ->
            %% It's not a minified file. Assume internal
            {jsfile, ScriptFile}
    end.

get_minified_file(Component, Tag, FilePaths, MinifierFun) ->
    MinifiedPath = case code:priv_dir(divapi) of
        {error, bad_name} -> <<"priv/js_minfied/">>;
        PrivDir           -> <<(list_to_binary(PrivDir))/binary, "/js_minified">>
    end,
    ComponentMinifiedPath = filename:join(MinifiedPath, <<Component/binary, "/", Tag/binary, "/">>),
    ok = filelib:ensure_dir(<<ComponentMinifiedPath/binary, "/">>),
    FilePath = filename:join(ComponentMinifiedPath, <<"scripts.min.js">>),
    case filelib:is_file(FilePath) of
        true ->
            {ok, FileBody} = file:read_file(FilePath),
            FileBody;
        false ->
            FileBody = MinifierFun(Component, Tag, FilePaths, <<>>),
            ok = file:write_file(FilePath, FileBody),
            FileBody
    end.

%% Fetch filec content from outside. And add it to the filecontent
get_file_contents(Component, Tag, [{external, ScriptFilePath} | Rest], FileContent) ->
    Opts = [{body_format, binary}],
    Request = {binary_to_list(ScriptFilePath), []},
    ScriptFile = case httpc:request(get, Request, [], Opts) of
        {ok, {{_Version, Status, _ReasonPhrase}, _Headers, Body}} ->

            case Status of
                404 -> throw({resource_not_found, ScriptFilePath});
                500 -> throw({server_error, ScriptFilePath});
                200 -> Body
            end
    end,
    get_file_contents(Component, Tag, Rest, <<FileContent/binary, ScriptFile/binary>>);
%% No action needed, add it to the file acc as is
get_file_contents(ComponentName, Tag, [{already_minified, ScriptFilePath} | Rest], FileContent) ->
    FileBody = git_utils:get_file(ComponentName, Tag, ScriptFilePath),
    get_file_contents(ComponentName, Tag, Rest, <<FileContent/binary, FileBody/binary>>);
%% Fetches filecontent for component and minify it through uglify
get_file_contents(ComponentName, Tag, [{jsfile, ScriptFilePath} | Rest], FileContent) ->
    TmpName0 = {ComponentName, Tag, ScriptFilePath},
    TmpName1 = integer_to_binary(erlang:phash2(TmpName0)),
    TmpPath = filename:join(<<"/tmp/divapi/">>, <<TmpName1/binary, ".js">>),
    File = git_utils:get_file(ComponentName, Tag, ScriptFilePath),
    ok = filelib:ensure_dir(TmpPath),
    ok = file:write_file(TmpPath, File),
    Command = <<"uglifyjs ", TmpPath/binary>>,
    Port = erlang:open_port({spawn, Command}, [exit_status, binary, stderr_to_stdout]),
    Result = wait_for_file(Port, <<>>),
    %% Cleanup
    file:delete(TmpPath),
    get_file_contents(ComponentName, Tag, Rest, <<FileContent/binary, Result/binary>>);
get_file_contents(_Component, _Tag, [], FileContent) ->
    FileContent.

wait_for_file(Port, File) ->
    receive
        {Port, {data, Chunk}} ->
            wait_for_file(Port, <<File/binary, Chunk/binary>>);
        {_ , {exit_status, 0}} ->
            File;
        {_, {exit_status, _}} ->
            throw({wait_for_response, failure})
    end.

pmap(Fun, List) ->
    pmap(Fun, List, 5000).

pmap(Fun, List, Timeout) ->
    Parent = self(),

    %% Spawn processes
    MapFun = fun (Item) -> spawn_monitor(fun () -> Parent ! {self(), Fun(Item)} end) end,
    Processes = lists:map(MapFun, List),

    %% Await results
    ReceiveFun = fun ({Pid, Reference}) ->
                         receive
                             {Pid, Result} ->
                                 Result;
                             {'DOWN', Reference, process, Pid, Reason} ->
                                 error({pmap, Pid, Reason})
                         after Timeout ->
                                   error({pmap, Pid, timeout})
                         end
                 end,
    lists:map(ReceiveFun, Processes).
