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
            %% Try to expand the tag into an existing one
            try expand_tag(PartialTag, get_tags(ComponentName)) of
                Tag ->
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
                    end
            catch
                error:_ ->
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

%% @doc Expand an incoming tag into the latest matching tag
%% Supported formats are:
%% *     - Latest version
%% X     - Latest minor in given major
%% X.Y   - Latest patch in given major and minor
%% X.Y.Z - Given version if it is in the given Tags
%%
%% all cases will throw an error if no version can be found
expand_tag(Tag0, Tags) ->
    case binary:split(Tag0, <<$.>>, [global]) of
        %% Exact version specified, check if it exists
        [_Major, _Minor, _Patch] = Version ->
            _Version = to_version(Version),
            true = lists:member(Tag0, Tags),
            Tag0;
        %% Expand to the latest version
        [<<"*">>] ->
            find_latest_version(fun compare_all/2, Tags);
        %% Expand to the latest minor and patch in a given major
        [Major0] ->
            Major1 = binary_to_integer(Major0),
            find_latest_version(compare_major(Major1), Tags);
        %% Expand to the latest tag in a given major and minor
        [Major0, Minor0] ->
            {Major1, Minor1} = {binary_to_integer(Major0), binary_to_integer(Minor0)},
            find_latest_version(compare_minor(Major1, Minor1), Tags)
    end.

%% @doc Compare all version
compare_all(Version0, Version1) ->
    case Version1 > Version0 of
        true  -> Version1;
        false -> Version0
    end.

%% @doc Compare only a given major version
compare_major(Major) ->
    %% Same major version, compare them
    fun ({_Major0, _Minor0, _Patch0} = Version0, {Major1, _Minor1, _Patch1} = Version1)
          when Major1 == Major ->
        case Version1 > Version0 of
            true  -> Version1;
            false -> Version0
        end;
    %% Different major, keep the acc
        (Version0, _Version1) ->
            Version0
    end.

%% @doc Compare a given major and minor version
compare_minor(Major, Minor) ->
    %% Same major version, compare them
    fun ({_Major0, _Minor0, _Patch0} = Version0, {Major1, Minor1, _Patch1} = Version1)
          when Major1 == Major, Minor1 == Minor ->
        case Version1 > Version0 of
            true  -> Version1;
            false -> Version0
        end;
    %% Different major, keep the acc
        (Version0, _Version1) ->
            Version0
    end.

%% @doc Retrive the latest version according to the given comparsion function
find_latest_version(MaybeSwap, Tags) ->
    find_latest_version({-1, -1, -1}, MaybeSwap, Tags).

find_latest_version(Version, _MaybeSwap, []) ->
    version_to_binary(Version);
find_latest_version(VersionAcc, MaybeSwap, [Tag | Tags]) ->
    NewVersion = try to_version(Tag) of
        %% Compare with the version we got
        Version -> MaybeSwap(VersionAcc, Version)
    catch
        %% Incorrect semver, ignore and move on
        error:_ -> VersionAcc
    end,
    find_latest_version(NewVersion, MaybeSwap, Tags).

%% @doc Version to binary
%% May throw error:badarg.
version_to_binary({Major, Minor, Patch}) when Major >= 0, Minor >= 0, Patch >= 0 ->
    <<(integer_to_binary(Major))/binary, $.,
      (integer_to_binary(Minor))/binary, $.,
      (integer_to_binary(Patch))/binary>>.

%% @doc Binary or list to version tuple
to_version(Version) when is_list(Version) ->
    case lists:map(fun binary_to_integer/1, Version) of
        [Major, Minor, Patch] when Major >= 0, Minor >= 0, Patch >= 0 ->
            {Major, Minor, Patch};
        _ ->
            error(badarg)
    end;
to_version(Tag) when is_binary(Tag) ->
    to_version(binary:split(Tag, <<$.>>, [global])).

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

expand_tag_test_() ->
    L = lists:seq(0,3),
    Tags = [<<"invalid">> | [version_to_binary({X,Y,Z}) || X <- L, Y <- L, Z <- L]],
    [?_assertEqual(<<"3.3.3">>, expand_tag(<<"*">>, Tags)),
     ?_assertEqual(<<"1.3.3">>, expand_tag(<<"1">>, Tags)),
     ?_assertEqual(<<"2.3.3">>, expand_tag(<<"2.3">>, Tags)),
     ?_assertEqual(<<"1.2.3">>, expand_tag(<<"1.2.3">>, Tags)),
     ?_assertError(_,           expand_tag(<<"4">>, Tags)),
     ?_assertError(_,           expand_tag(<<"3.4">>, Tags)),
     ?_assertError(_,           expand_tag(<<"*">>, [<<"no_valid_tags">>]))].

-endif.
