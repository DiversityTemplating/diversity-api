-module(divapi_js_minifier).

-export([minify/1, minify/2, get_minified_tags/1]).

%% @doc Minifies all scripts and concatenates it to one file for all tagged versions not
%%      already minfied.
minify(ComponentName) ->
    ComponentTags = git_utils:tags(ComponentName),
    MinifiedTags = get_minified_tags(ComponentName),
    lists:foreach(
        fun (Tag) ->
           case lists:member(Tag, MinifiedTags) of
               false -> minify(ComponentName, Tag);
               true  -> ok
           end
        end,
        ComponentTags
    ),
    ok.

%% @doc Minifies all scripts for a specific component and tag. Returns minified file.
minify(_Component, <<"*">>) ->
    throw(not_allowed);
minify(ComponentName, Tag) ->
    ScriptFilePaths = get_component_script_files(ComponentName, Tag),
    minify(ComponentName, Tag, ScriptFilePaths).

%% @doc Minifies all scripts for a specific component and tag. Returns minified file.
%%      If component/tag already minfied, that result will be returned.
minify(ComponentName, Tag, ScriptFilePaths) ->
    MinifiedPath = get_minify_path(ComponentName),
    ComponentMinifiedPath = filename:join(MinifiedPath, <<Tag/binary, "/">>),
    FilePath = filename:join(ComponentMinifiedPath, <<"scripts.min.js">>),
    case file:read_file(FilePath) of
        {ok, FileBody} ->
            FileBody;
        {error, enoent}->
            ok = filelib:ensure_dir(<<ComponentMinifiedPath/binary, "/">>),
            FileBody = minify(ComponentName, Tag, ScriptFilePaths, <<>>),
            ok = file:write_file(FilePath, FileBody),
            FileBody
    end.

%% Fetch filec content from outside. And add it to the filecontent
minify(ComponentName, Tag, [{external, ScriptFilePath} | Rest], FileContent) ->
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
    minify(ComponentName, Tag, Rest, <<FileContent/binary, ";", ScriptFile/binary>>);
%% No action needed, add it to the file acc as is
minify(ComponentName, Tag, [{already_minified, ScriptFilePath} | Rest], FileContent) ->
    FileBody = git_utils:get_file(ComponentName, Tag, ScriptFilePath),
    minify(ComponentName, Tag, Rest, <<FileContent/binary, ";", FileBody/binary>>);
%% Fetches filecontent for component and minify it through uglify
minify(ComponentName, Tag, [{jsfile, ScriptFilePath} | Rest], FileContent) ->
    TmpPath = filename:join(<<"/tmp/divapi/">>, ScriptFilePath),
    File = git_utils:get_file(ComponentName, Tag, ScriptFilePath),
    ok = filelib:ensure_dir(TmpPath),
    ok = file:write_file(TmpPath, File),
    Command = <<"uglifyjs ", TmpPath/binary>>,
    Port = erlang:open_port({spawn, Command}, [exit_status, binary, stderr_to_stdout]),
    Result = wait_for_file(Port, <<>>),
    %% Cleanup
    file:delete(TmpPath),
    minify(ComponentName, Tag, Rest, <<FileContent/binary, ";", Result/binary>>);
minify(_Component, _Tag, [], FileContent) ->
    FileContent.

%% @doc Returns all scriptfiles listed in the diversity.json setting. It will tag
%%      each file if it's external, jsfile or already_minified.
get_component_script_files(ComponentName, Tag) ->
    {DiversityProps} = divapi_component:diversity_json(ComponentName, Tag),
    ScriptFiles = proplists:get_value(<<"script">>, DiversityProps),
    lists:map(fun get_file_type/1, ScriptFiles).

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

%% @doc Fetches a list of already minified tags for a specific component.
get_minified_tags(ComponentName) ->
    MinifiedPath = get_minify_path(ComponentName),
    case file:list_dir(MinifiedPath) of
        {ok, TagList} -> lists:map(fun(Tag) -> unicode:characters_to_binary(Tag) end, TagList);
        {error, _}    -> []
    end.

%% @doc Returns path to where each minified file is stored for component.
get_minify_path(ComponentName) ->
    MinifiedPath = case application:get_env(divapi, component_cache) of
        undefined  -> <<"/var/diversity-api/js_minified">>;
        {ok, Dir}  -> <<(unicode:characters_to_binary(Dir))/binary, "/js_minified">>
    end,
    filename:join(MinifiedPath, <<ComponentName/binary, "/">>).

wait_for_file(Port, File) ->
    receive
        {Port, {data, Chunk}} ->
            wait_for_file(Port, <<File/binary, Chunk/binary>>);
        {_ , {exit_status, 0}} ->
            File;
        {_, {exit_status, _}} ->
            throw({wait_for_response, failure})
    end.
