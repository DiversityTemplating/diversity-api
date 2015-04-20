-module(divapi_component).

-export([tags/1, ]).

%% @doc Retrive all tags for a given component
tags(ComponentName) ->
    git_utils:tags(ComponentName).

%% @doc Serve all tags for a given component.
serve_tags(ComponentName) ->
    {json, jiffy:encode(tags(ComponentName))}.

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
