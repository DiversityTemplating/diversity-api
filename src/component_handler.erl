-module(component_handler).

-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {}).

-define(JSON_HEADER, [{<<"content-type">>, <<"application/json">>}]).
-define(CSS_HEADER, [{<<"content-type">>, <<"text/css">>}]).
-define(ACCESS_CONTROL_HEADER, [{<<"Access-Control-Allow-Origin">>, <<"*">>}]).


%%
%% Cowboy callbacks
%%

init(_, Req, _Opts) ->
    {ok, Req, #state{}}.

handle(Req, State=#state{}) ->
    {Method, Req2} = cowboy_req:method(Req),
    {ok, Req3} =
        case Method of
            <<"GET">> -> handle_get(Req2, State);
            _         -> reply_from_result_data(Req2, resource_not_found)
        end,
    {ok, Req3, State}.

%% @doc Handle a GET-request
handle_get(Req, _State=#state{}) ->
    {ComponentName, Req1} = cowboy_req:binding(component, Req),
    case component_exists(ComponentName) of
        true ->
            handle_component_request(Req1, ComponentName);
        false ->
            reply_from_result_data(Req1, resource_not_found)
    end.

%% @doc Check if a component is exists in the repository directory
component_exists(ComponentName) ->
    {ok, RepoDir} = application:get_env(divapi, repo_dir),
    filelib:is_dir(RepoDir ++ "/" ++ binary_to_list(ComponentName) ++ ".git").

%% @doc Handle a component-request
%% The possible routes to this function is:
%% /ComponentName/                    - List all tags
%% /ComponentName/Tag/                - Serve diversity.json
%% /ComponentName/Tag/files/some.file - Serve a file
%% /ComponentName/Tag/css             - Serve concatenated CSS
%% /ComponentName/Tag/thumbnail       - Serve thumbnail
handle_component_request(Req, ComponentName) ->
    {PathInfo, Req1} = cowboy_req:path_info(Req),
    ResultData = case PathInfo of
        undefined ->
            serve_tags(ComponentName);
        [PartialTag | Routes] ->
            Tag = expand_tag(PartialTag, get_tags(ComponentName)),
            case Routes of
                [] ->
                    serve_diversity_json(ComponentName, Tag);
                [Settings] when Settings =:= <<"settings">>;
                                Settings =:= <<"settingsForm">> ->
                    serve_setting_json(ComponentName, Tag, Settings);
                [<<"files">> | Path] ->
                    File = filename:join(Path),
                    {BrowserCacheKey, _} = cowboy_req:header(<<"if-none-match">>, Req1),
                    serve_file(ComponentName, Tag, File, BrowserCacheKey);
                [<<"css">>] ->
                    %% Retrive additional variables and sort them to get a
                    %% consistent cache key
                    {Variables, _Req2} = cowboy_req:qs_vals(Req1),
                    serve_css(ComponentName, Tag, Variables);
                [<<"thumbnail">>] ->
                    {BrowserCacheKey, _} = cowboy_req:header(<<"if-none-match">>, Req1),
                    serve_thumbnail(ComponentName, BrowserCacheKey);
                _ ->
                    resource_not_found
            end;
        _ ->
            resource_not_found
    end,
    reply_from_result_data(Req1, ResultData).

%% @doc Contstruct the cowboy-reply
reply_from_result_data(Req, file_not_changed) ->
    cowboy_req:reply(304, ?ACCESS_CONTROL_HEADER, Req);
reply_from_result_data(Req, resource_not_found) ->
    cowboy_req:reply(404, Req);
reply_from_result_data(Req, {error, Reason}) ->
    cowboy_req:reply(500, ?ACCESS_CONTROL_HEADER, Reason, Req);
reply_from_result_data(Req, {ok, {Headers, Data}}) ->
    cowboy_req:reply(200, ?ACCESS_CONTROL_HEADER ++ Headers, Data, Req);
reply_from_result_data(Req, {json, Data}) ->
    cowboy_req:reply(200, ?ACCESS_CONTROL_HEADER ++ ?JSON_HEADER, Data, Req);
reply_from_result_data(Req, {css, Data}) ->
    cowboy_req:reply(200, ?ACCESS_CONTROL_HEADER ++ ?CSS_HEADER, Data, Req).

%% @doc Retrive all tags for a given component
get_tags(ComponentName) ->
    git_utils:tags(ComponentName).

%% @doc Serve all tags for a given component.
serve_tags(ComponentName) ->
    {json, jiffy:encode(get_tags(ComponentName))}.

%% @doc Serve diversity.json for a given component and tag.
serve_diversity_json(ComponentName, Tag) ->
    case git_utils:get_diversity_json(ComponentName, Tag) of
        undefined -> resource_not_found;
        Data      -> {json, Data}
    end.

%% @doc Serve the settings/settingsForm from diversity.json
serve_setting_json(ComponentName, Tag, Settings) ->
    case git_utils:get_diversity_json(ComponentName, Tag) of
        undefined ->
            resource_not_found;
        Json ->
            {DiversityData} = jiffy:decode(Json),
            SettingsBinary = proplists:get_value(Settings, DiversityData, {[]}),
            SettingsJson = jiffy:encode(SettingsBinary),
            {json, SettingsJson}
    end.

%% @doc Serve an arbitrary file from the repository
serve_file(_ComponentName, Tag, File, BrowserCacheKey) when <<Tag/binary, File/binary>> ==
                                                            BrowserCacheKey ->
    file_not_changed;
serve_file(ComponentName, Tag, File, _BrowserCacheKey) ->
    case git_utils:get_file(ComponentName, Tag, File) of
        undefined ->
            resource_not_found;
        error ->
            %% Should get a reason from git_utils get_file command.
            {error, <<"Error while fetching file">>};
        FileBin ->
            {Mime, Type, []} = cow_mimetypes:all(File),
            Headers = [{<<"content-type">>,
                        << Mime/binary, "/", Type/binary >>}],
            Headers1 = case Tag of
                <<"HEAD">> -> Headers;
                _          -> Headers ++ [{<<"Cache-Control">>, <<"max-age=90000">>},
                                          {<<"Etag">>, <<Tag/binary, File/binary>>}]
            end,
            {ok, {Headers1, FileBin}}
    end.

%% @doc Serve all css and sass (concatenated into one file)
%% This function will check out the diversity.json-file from the components repository
%% and check the styles-property to find all the css- and sass-files. The sass files are
%% first compiled then all css is concatenated and returned to the user.
serve_css(ComponentName, Tag, Variables0) ->
    case git_utils:get_diversity_json(ComponentName, Tag) of
        undefined ->
            resource_not_found;
        DiversityData ->
            {DiversityJSON} = jiffy:decode(DiversityData),

            %% The style property may be either a single file or a list of files
            StylePaths = case proplists:get_value(<<"style">>, DiversityJSON, []) of
                Path  when is_binary(Path) -> [Path];
                Paths when is_list(Paths)  -> Paths
            end,

            %% To get a consitent cache key we need to make sure the proplist is sorted
            Variables1 = lists:sort(Variables0),
            Files = divapi_cache:get(
                {css, ComponentName, Tag, Variables1},
                fun () -> get_css(ComponentName, Tag, Variables1, StylePaths) end,
                1000 * 60 * 60 * 5 % 5 hours
             ),

            %% Return them as an iolist() (concatenated)
            {css, Files}
    end.

serve_thumbnail(ComponentName, BrowserCacheKey) ->
    case git_utils:get_diversity_json(ComponentName, <<"*">>) of
        undefined ->
            resource_not_found;
        Json ->
            {DiversityData} = jiffy:decode(Json),
            ThumbnailPath = proplists:get_value(<<"thumbnail">>, DiversityData, undefined),
            serve_file(ComponentName, <<"*">>, ThumbnailPath, BrowserCacheKey)
    end.



%% @doc Retrive al
get_css(ComponentName, Tag, Variables, Paths) ->
    %% Retrive all files(CSS and SCSS).
    %% Compile all SCSS-files to CSS and then concatenate all of them
    GetFile = fun (Path) ->
        %% Retrive the temporary checked out file from the repository
        File = git_utils:get_file(ComponentName, Tag, Path),
        case filename:extension(Path) of
            <<".scss">> ->
                sass_compile(ComponentName, Tag, Path, File, Variables);
            <<".css">> ->
                File
        end
    end,
    lists:map(GetFile, Paths).


%% @doc Compile a sass-file with given variables
sass_compile(ComponentName, Tag, Path, File, Variables) ->

    %% Construct the SASS-variables
    VarToBinary = fun ({Variable, Value}, BinAcc) ->
                      BinVar = <<"$", Variable/binary, ": ", Value/binary, "; ">>,
                      <<BinAcc/binary, BinVar/binary>>
                  end,
    BinaryVars = lists:foldl(VarToBinary, <<>>, Variables),

    %% Create a temporary sass file
    TmpName0 = {ComponentName, Tag, Path, Variables},
    TmpName1 = integer_to_binary(erlang:phash2(TmpName0)),
    TmpPath = filename:join(<<"/tmp/divapi/">>, TmpName1),
    ok = filelib:ensure_dir(TmpPath),
    ok = file:write_file(TmpPath, <<BinaryVars/binary, (iolist_to_binary(File))/binary>>),

    %% Compile it
    PrivDir = unicode:characters_to_binary(code:priv_dir(divapi)),
    SassC = filename:join(PrivDir, <<"sassc">>),
    Command = <<SassC/binary, " -t compressed ", TmpPath/binary>>,
    Port = erlang:open_port({spawn, Command}, [exit_status, binary, stderr_to_stdout]),
    Result = wait_for_response(Port, <<>>),

    %% Cleanup
    file:delete(TmpPath),

    Result.

wait_for_response(Port, File) ->
    receive
        {Port, {data, Chunk}} ->
            wait_for_response(Port, <<File/binary, Chunk/binary>>);
        {_ , {exit_status, 0}} ->
            File;
        {_, {exit_status, _}} ->
            %% Either not a git repo or operation failed. No need to close port it' already done.
            error
    end.


expand_tag(<<"*">>, _Tags) -> <<"HEAD">>;
expand_tag(Tag, Tags) ->
    case lists:member(Tag, Tags) of
        true ->
            Tag;
        false ->
            PatchNr = find_latest_patch(Tag, Tags),
            <<Tag/binary, ".", PatchNr/binary>>
    end.

find_latest_patch(PrefixTag, Tags) ->
    lists:foldl(fun(Tag, LatestPatch) ->
        case binary:longest_common_prefix([Tag, PrefixTag])of
            3 ->
                %% <<"1.2.3">>, the patch nr is char 4
                PatchNr = binary:part(Tag, 4, 1),
                case LatestPatch =< PatchNr of
                    true -> PatchNr;
                    false -> LatestPatch
                end;
            _ -> LatestPatch
        end
    end, <<"0">>, Tags).

terminate(_Reason, _Req, _State) ->
    ok.

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

%% Testdata
-define(REPO, <<"test-component">>).
-define(TAG,  <<"0.0.1">>).
-define(TEST_CSS, <<".body { font-family: test; }">>).
-define(TEST_SCSS, <<"$test-color: #FFFFFF\n.test { background: $test-color; }">>).
-define(DIVERSITY_JSON, {[{style, [<<"test.css">>, <<"test.scss">>]}]}).

serve_css_test() ->
    {setup,
     fun() ->
        %% Mock the git_utils module
        meck:new(git_utils),
        meck:expect(
            git_utils, get_diversity_json,
            fun (?REPO, ?TAG) -> jiffy:encode(?DIVERSITY_JSON) end
        ),
        meck:expect(
            git_utils, get_file,
            fun (?REPO, ?TAG, "test.css")  -> ?TEST_CSS;
                (?REPO, ?TAG, "test.scss") -> ?TEST_SCSS
            end
        ),
        application:start(divapi)
     end,
     fun(_) ->
         application:stop(divapi),
         meck:unload(git_utils)
     end,
     [
      {"Serve CSS files",
       fun () ->
           Variables = [{<<"test-color">>, <<"#000000">>}],
           Expected =
               <<
                 %% The css should just be concatenated
                 ?TEST_CSS/binary,

                 %% The scss should be compiled with the given
                 %% variables and then concatenated
                 ".test { background: #000000; }"
               >>,
          ?assertEqual({css, Expected}, serve_css(?REPO, ?TAG, Variables))
       end}
     ]
    }.

-endif.
