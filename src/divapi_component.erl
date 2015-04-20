-module(divapi_component).

-export([tags/1, diversity_json/2, settings/2, settingsForm/2, file/3, css/3, thumbnail/1]).

%% @doc Retrive all tags for a given component
tags(ComponentName) ->
    git_utils:tags(ComponentName).

%% @doc Fetches the diversity json for a spcific component/tag. Will cache the result.
diversity_json(ComponentName, Tag) ->
    divapi_cache:get(
        {diversity_json, ComponentName, Tag},
        fun () -> jiffy:decode(git_utils:get_diversity_json(ComponentName, Tag)) end,
        1000 * 60 * 60 * 5 % 5 hours
     ).

%% @doc Serve the settings from diversity.json
settings(ComponentName, Tag) ->
    case diversity_json(ComponentName, Tag) of
        undefined ->
            resource_not_found;
        Json ->
            {DiversityData} = jiffy:decode(Json),
            SettingsBinary = proplists:get_value(<<"settings">>, DiversityData, {[]}),
            SettingsJson = jiffy:encode(SettingsBinary),
            SettingsJson
    end.

%% @doc Serve the settingsForm from diversity.json
settingsForm(ComponentName, Tag) ->
    case diversity_json(ComponentName, Tag) of
        undefined ->
            resource_not_found;
        Json ->
            {DiversityData} = jiffy:decode(Json),
            SettingsBinary = proplists:get_value(<<"settingsForm">>, DiversityData, {[]}),
            SettingsJson = jiffy:encode(SettingsBinary),
            SettingsJson
    end.


%% @doc Serve an arbitrary file from the repository

file(ComponentName, Tag, File) ->
    case git_utils:get_file(ComponentName, Tag, File) of
        undefined ->
            resource_not_found;
        error ->
            %% Should get a reason from git_utils get_file command.
            {error, <<"Error while fetching file">>};
        FileBin ->
            FileBin
    end.

%% @doc Serve all css and sass (concatenated into one file)
%% This function will check out the diversity.json-file from the components repository
%% and check the styles-property to find all the css- and sass-files. The sass files are
%% first compiled then all css is concatenated and returned to the user.
css(ComponentName, Tag, Variables0) ->
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
            {css, Files}
    end.

thumbnail(ComponentName) ->
    case git_utils:get_diversity_json(ComponentName, <<"*">>) of
        undefined ->
            resource_not_found;
        Json ->
            {DiversityData} = jiffy:decode(Json),
            ThumbnailPath = proplists:get_value(<<"thumbnail">>, DiversityData, undefined),
            file(ComponentName, <<"*">>, ThumbnailPath)
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
          ?assertEqual({css, Expected}, css(?REPO, ?TAG, Variables))
       end}
     ]
    }.

-endif.
