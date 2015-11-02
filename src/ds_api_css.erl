-module(ds_api_css).

-export([minify/1]).
-export([compile/3]).
-export([compile_component_sass/5]).
-export([style_files/2]).
-export([variables_to_binary/1]).

minify(CSSFile) ->
    MinifiedFile = minified_name(CSSFile),
    minify(CSSFile, MinifiedFile).

minify(Input, Output) ->
    SassC = sassc_path(),
    Command = <<SassC/binary, " -t compressed ", Input/binary, " ", Output/binary>>,
    WorkingDir = filename:dirname(Input),
    case ds_api_util:cmd(Command, WorkingDir) of
        {ok, _Reply} -> {ok, Output};
        Error        -> Error
    end.

%% @doc Compile a SASS-file with a given include path
compile(LoadPath, Input, Output) ->
    SassC = sassc_path(),
    WorkingDir = filename:dirname(Output),
    InputFile = <<Output/binary, ".input">>,
    Command = <<SassC/binary, " -t compressed -I ", LoadPath/binary,
                " ", InputFile/binary, " ", Output/binary>>,
    ok = file:write_file(InputFile, Input),
    ds_api_util:cmd(Command, WorkingDir).

compile_component_sass(Component, Version, VarHash, Variables, CSSFiles0) ->
    VersionDir = ds_api_component_mgr:version_dir(Component, Version),
    FilesDir = ds_api_component_mgr:files_dir(Component, Version),
    LoadPath = filename:join(FilesDir, styles),
    TempDir = ds_api_component_mgr:temp_dir(Component, Version),
    SASSDir = ds_api_component_mgr:sass_dir(Component, Version),
    CSSFiles1 = compile_component_sass(VersionDir, TempDir, SASSDir, LoadPath, VarHash, Variables, CSSFiles0, []),
    ds_api_util:delete_dir(TempDir),
    CSSFiles1.

compile_component_sass(_VersionDir, _TempDir, _SASSDir, _LoadPath, _VarHash, _Variables, [], Acc) ->
    {ok, lists:reverse(Acc)};
compile_component_sass(VersionDir, TempDir, SASSDir, LoadPath, VarHash, Variables, [CSSFile | CSSFiles], Acc0) ->
    case filename:extension(CSSFile) of
        <<".css">> ->
            Acc1 = [CSSFile | Acc0],
            compile_component_sass(VersionDir, TempDir, SASSDir, LoadPath, VarHash, Variables, CSSFiles, Acc1);
        <<".scss">> ->
            VersionLocalFile = get_version_local_path(VersionDir, CSSFile),
            TempOutputFile = filename:join(TempDir, VersionLocalFile),
            ok = filelib:ensure_dir(TempOutputFile),
            {ok, Input} = file:read_file(CSSFile),
            ok = compile(LoadPath, <<Variables/binary, Input/binary>>, TempOutputFile),

            OutputFile = filename:join([SASSDir, VersionLocalFile, VarHash]),
            ok = filelib:ensure_dir(OutputFile),
            ok = file:rename(TempOutputFile, OutputFile),
            ok = ds_api_util:gzip(OutputFile),
            Acc1 = [OutputFile | Acc0],
            compile_component_sass(VersionDir, TempDir, SASSDir, LoadPath, VarHash, Variables, CSSFiles, Acc1)
    end.

get_version_local_path(VersionDir, File) ->
    VersionDirSize = byte_size(VersionDir),
    <<_:VersionDirSize/binary, $/, VersionLocalFile/binary>> = File,
    VersionLocalFile.

style_files(Component, Version) ->
    VersionDir = ds_api_component:version_dir(Component, Version),
    case ds_api_component:diversity_json(Component, Version, VersionDir) of
        {ok, Diversity} -> ds_api_component:diversity_json_files(<<"style">>, Diversity, VersionDir);
        undefined       -> undefined
    end.

minified_name(CSSFile) ->
    ds_api_util:minified_name(CSSFile, <<".css">>).

sassc_path() ->
    filename:join(unicode:characters_to_binary(code:priv_dir(ds_api)), <<"sassc">>).

variables_to_binary(QS) ->
    variables_to_binary(QS, <<>>).

variables_to_binary([], Acc) ->
    Acc;
variables_to_binary([{Variable, Value} | Variables], Acc) ->
    variables_to_binary(Variables, <<Acc/binary, Variable/binary, $:, Value/binary, $;>>).
