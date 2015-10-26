-module(ds_api_js).

-define(DEFAULT_MINIFY_TIMEOUT, 60000).

-export([minify/1]).

minify(JSFile) ->
    RootName = filename:rootname(JSFile, <<".js">>),
    MinifiedFile = <<RootName/binary, ".min.js">>,
    lager:debug(
      "MINIFYING JS~n"
      "INPUT: ~p~n"
      "OUTPUT: ~p~n",
      [JSFile, MinifiedFile]
     ),
    minify(JSFile, MinifiedFile).

minify(Input, Output) ->
    Command = <<"uglifyjs --screw-ie8 -c -m -o ", Output/binary, " ", Input/binary>>,
    WorkingDir = filename:dirname(Input),
    case ds_api_util:cmd(Command, WorkingDir) of
        {ok, _Reply} -> {ok, Output};
        Error        -> Error
    end.

%%% @doc Minifies all javascripts for a given component
%minify(ComponentName) ->
%    Tags = ds_api_component:get_tags(ComponentName),
%    [minify(ComponentName, Tag) || Tag <- Tags],
%    ok.
%
%%% @doc Minifies all scripts for a specific component and tag.
%%%      If component/tag already minfied, that result will be returned.
%minify(Component, Tag) ->
%    ScriptMinFile = get_component_minified_file_path(Component, Tag),
%    case file:read_file(ScriptMinFile) of
%        {ok, FileBody} ->
%            FileBody;
%        {error, enoent}->
%            MinifiedPath = get_component_minified_path(Component, Tag),
%            ok = filelib:ensure_dir(MinifiedPath),
%            ok = do_minify(Component, Tag)
%    end.
%
%%% @doc Fetch all remote script file and save them temporarily to disk then
%%% compile all javascript into a single file
%do_minify(Component, Tag) ->
%    TaggedScriptFilePaths = get_component_script_files(Component, Tag),
%    LocalScriptFilePaths = fetch_script_files(TaggedScriptFilePaths),
%    MinifiedScriptFilePaths = do_minify(LocalScriptFilePaths),
%    concatenate_script_files(Component, Tag, MinifiedScriptFilePaths).
%
%do_minify(ScriptFiles) ->
%    [case is_minified(ScriptFile) of
%         true ->
%             ScriptFile;
%         false ->
%             {ok, MinifiedScriptFile} = do_minify_file(ScriptFile),
%             MinifiedScriptFile
%     end || ScriptFile <- ScriptFiles].
%
%do_minify_file(ScriptFile) ->
%    RootName = filename:rootname(ScriptFile, <<".js">>),
%    MinifiedScriptFile = <<RootName/binary, ".min.js">>,
%
%    Args = [ScriptFile, <<"-o">>, MinifiedScriptFile],
%    Opts = [exit_status, binary, in, {args, Args}],
%    Port = erlang:open_port({spawn_executable, <<"uglifyjs">>}, Opts),
%    MinifyTimeout = ds_api:config(minify_timeout, ?DEFAULT_MINIFY_TIMEOUT),
%    receive
%        {exit_status, 0}         -> {ok, MinifiedScriptFile};
%        {exit_status, ErrorCode} -> {error, ErrorCode}
%    after MinifyTimeout ->
%        true = erlang:port_close(Port),
%        {error, timeout}
%    end.
%
%concatenate_script_files(Component, Tag, MinifiedScriptFiles) ->
%    MinifiedFilePath = get_component_minified_file_path(Component, Tag),
%    {ok, File} = file:open(MinifiedFilePath, [append]),
%    lists:foreach(
%      fun (MinifiedScriptFile) ->
%          {ok, MinifiedScriptData} = file:read_file(MinifiedScriptFile),
%          ok = file:write(File, MinifiedScriptData),
%          ok = file:write(File, <<$;>>)
%      end,
%      MinifiedScriptFiles
%    ),
%    ok = file:close(File).
%
%%% @doc Get a tagged list of all script files contained in a tag for a component
%get_component_script_files(Component, Tag) ->
%    case ds_api_component:diversity_json(Component, Tag) of
%        {ok, Diversity} ->
%            Scripts = maps:get(<<"script">>, Diversity, []),
%            {ok, [expand_file_path(Component, Tag, Script) || Script <- Scripts]};
%        Error ->
%            Error
%    end.
%
%%% @doc Fetch all remote script files and save them locally then return the local
%%% path to the file. Local paths are just returned
%fetch_script_files(ScriptFiles) ->
%    [fetch_script_file(ScriptFile) || ScriptFile <- ScriptFiles].
%
%fetch_script_file({remote, URL, LocalPath}) ->
%    case filelib:is_file(LocalPath) of
%        true ->
%            ok;
%        false ->
%            Opts = [{body_format, binary}],
%            Request = {unicode:characters_to_list(URL), []},
%            {ok, Reply} = httpc:request(get, Request, [], Opts),
%            {{_Version, 200, _Status}, _Headers, Body} = Reply,
%            ok = filelib:ensure_dir(LocalPath),
%            ok = file:write_file(LocalPath, Body)
%    end,
%    LocalPath;
%fetch_script_file(LocalPath) when is_binary(LocalPath) ->
%    LocalPath.
%
%expand_file_path(Component, Tag, <<"//", URL/binary>> = ScriptFile) ->
%    {remote, <<"http:", ScriptFile/binary>>, url_to_path(Component, Tag, URL)};
%expand_file_path(Component, Tag, <<"http://", URL/binary>> = ScriptFile) ->
%    {remote, ScriptFile, url_to_path(Component, Tag, URL)};
%expand_file_path(Component, Tag, <<"https://", URL/binary>> = ScriptFile) ->
%    {remote, ScriptFile, url_to_path(Component, Tag, URL)};
%expand_file_path(Component, Tag, ScriptFile) ->
%    ds_api_component:path(Component, Tag, ScriptFile).
%
%url_to_path(Component, Tag, URL) ->
%    MinifiedPath = get_component_minified_path(Component, Tag),
%    filename:join(MinifiedPath, URL).
%
%get_component_minified_path(Component, Tag) ->
%    filename:join([ds_api:components_dir(), Component, Tag]).
%
%get_component_minified_file_path(Component, Tag) ->
%    filename:join(get_component_minified_path(Component, Tag), <<"script.min.js">>).
%
%is_minified(ScriptFile) ->
%    MinifiedSuffix = <<".min.js">>,
%    binary:longest_common_suffix([ScriptFile, MinifiedSuffix]) =:= byte_size(MinifiedSuffix).
