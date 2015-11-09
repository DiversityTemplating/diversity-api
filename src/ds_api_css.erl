-module(ds_api_css).

-export([minify/1]).
-export([compile/3]).
-export([compile_and_concatenate/1]).
-export([compile_and_concatenate/2]).

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
compile(LoadPath, Input, OutputFile) ->
    SassC = sassc_path(),
    WorkingDir = filename:dirname(OutputFile),
    InputFile = <<OutputFile/binary, ".input">>,
    Command = <<SassC/binary, " -t compressed -I ", LoadPath/binary,
                " ", InputFile/binary, " ", OutputFile/binary>>,
    ok = file:write_file(InputFile, Input),
    case ds_api_util:cmd(Command, WorkingDir) of
        {ok, _Output} -> ok;
        Error         -> Error
    end.

compile_and_concatenate(VersionDir) ->
    compile_and_concatenate(VersionDir, []).

compile_and_concatenate(VersionDir, Variables0) ->
    Variables1 = variables_to_binary(Variables0),
    Prefix = case Variables1 of
                 <<>> -> no_variables;
                 _    -> ds_api_util:hash(Variables1)
             end,
    SASSDir = ds_api_component:sass_dir(VersionDir),
    OutputDir = filename:join(SASSDir, Prefix),
    OutputFile = filename:join(OutputDir, <<"main.min.css">>),

    case filelib:is_regular(OutputFile) of
        true ->
            {ok, OutputFile};
        false ->
            case style_files(VersionDir) of
                CSSFiles0 when is_list(CSSFiles0) ->
                    FilesDir = ds_api_component:files_dir(VersionDir),
                    LoadPath = filename:join(FilesDir, styles),
                    TmpDir = ds_api_util:tmp_path(),
                    MainTmpDir = filename:join(TmpDir, main),
                    TmpOutputFile = filename:join(MainTmpDir, <<"main.min.css">>),
                    CompileTmpDir = filename:join(TmpDir, compiled),
                    ok = filelib:ensure_dir(TmpOutputFile),
                    Result = case compile(CompileTmpDir, LoadPath, Variables1, CSSFiles0, []) of
                                 {ok, CSSFiles1} -> 
                                     ok = ds_api_util:concatenate_files(CSSFiles1, TmpOutputFile),
                                     ok = ds_api_util:gzip(TmpOutputFile),
                                     ok = file:rename(MainTmpDir, OutputDir),
                                     {ok, OutputFile};
                                 Error ->
                                     Error
                             end,
                    ds_api_util:delete_dir(TmpDir),
                    Result;
                undefined ->
                    undefined
            end
    end.

compile(_CompileTmpDir, _LoadPath, _Variables, [], Acc) ->
    {ok, lists:reverse(Acc)};
compile(CompileTmpDir, LoadPath, Variables, [CSSFile0 | CSSFiles], Acc0) ->
    case filename:extension(CSSFile0) of
        <<".css">> ->
            compile(CompileTmpDir, LoadPath, Variables, CSSFiles, [CSSFile0 | Acc0]);
        <<".scss">> ->
            <<$/, CSSFile1/binary>> = CSSFile0,
            OutputFile = filename:join(CompileTmpDir, CSSFile1),
            ok = filelib:ensure_dir(OutputFile),
            {ok, Input} = file:read_file(CSSFile0),
            case compile(LoadPath, <<Variables/binary, Input/binary>>, OutputFile) of
                ok    -> compile(CompileTmpDir, LoadPath, Variables, CSSFiles, [OutputFile | Acc0]);
                Error -> Error
            end
    end.

style_files(VersionDir) ->
    FilesDir = ds_api_component:files_dir(VersionDir),
    DiversityPath = filename:join(FilesDir, <<"diversity.json">>),
    case ds_api_util:read_json_file(DiversityPath) of
        {ok, #{<<"style">> := Style}} when is_binary(Style); is_list(Style) ->
            CSSFiles0 = ds_api_preprocess:expand_file_names(Style, VersionDir),
            CSSFiles1 = ds_api_preprocess:local_file_names(CSSFiles0, VersionDir),
            [minified_name(CSSFile) || CSSFile <- CSSFiles1];
        _ ->
            undefined
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
    variables_to_binary(Variables, <<Acc/binary, $$, Variable/binary, $:, Value/binary, $;>>).
