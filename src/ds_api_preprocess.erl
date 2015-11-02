-module(ds_api_preprocess).

-export([run/3]).

run(Component, Version, TmpDir) ->
    DiversityPath = filename:join(TmpDir, <<"files/diversity.json">>),
    case file:read_file(DiversityPath) of
        {ok, Diversity0} ->
            Diversity1 = jiffy:decode(Diversity0),
            case check_diversity_json(Component, Version, Diversity1) of
                ok ->
                    ok = write_compact_diversity_json(DiversityPath, Diversity1),
                    pre_process_files(Diversity1, TmpDir);
                Error ->
                    Error
            end;
        _Error ->
            {error, <<"Could not read diversity.json.">>}
    end.

pre_process_files(Diversity, TmpDir) ->
    case pre_process_css_files(Diversity, TmpDir) of
        ok ->
            case pre_process_js_files(Diversity, TmpDir) of
                ok    -> gzip_all_files(TmpDir);
                Error -> Error
            end;
        Error ->
            Error
    end.

pre_process_css_files(Diversity, TmpDir) ->
    MaybeRemoteCSSFiles = ds_api_component:diversity_json_files(<<"style">>, Diversity, TmpDir),
    case fetch_remote_files(MaybeRemoteCSSFiles, TmpDir) of
        {ok, LocalCSSFiles} ->
            case minify_files(ds_api_css, <<".css">>, LocalCSSFiles) of
                {ok, _Minified} -> ok;
                Error           -> Error
            end;
        Error ->
            Error
    end.

pre_process_js_files(Diversity, TmpDir) ->
    MaybeRemoteJSFiles = ds_api_component:diversity_json_files(<<"script">>, Diversity, TmpDir),
    case fetch_remote_files(MaybeRemoteJSFiles, TmpDir) of
        {ok, LocalJSFiles} ->
            case minify_files(ds_api_js, <<".js">>, LocalJSFiles) of
                {ok, MinifiedJSFiles} ->
                    OutputFile = filename:join(TmpDir, <<"script.min.js">>),
                    concatenate_files(MinifiedJSFiles, OutputFile);
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

minify_files(Module, Extension, Files) ->
    minify_files(Module, Extension, Files, []).

minify_files(_Module, _Extension, [], Acc) ->
    {ok, lists:reverse(Acc)};
minify_files(Module, Extension, [File0 | Files], Acc) ->
    %% Check the file type
    case filename:extension(File0) of
        %% If it's a minifiable file
        Extension ->
            File1 = filename:rootname(File0, Extension),
            %% Check if it's already minified
            case filename:extension(File1) of
                %% If it is, just skip it
                <<".min">> ->
                    minify_files(Module, Extension, Files, [File0 | Acc]);
                %% Otherwise minify it
                _ ->
                    case Module:minify(File0) of
                        {ok, Minified} ->
                            minify_files(Module, Extension, Files, [Minified | Acc]);
                        Error ->
                            Error
                    end
            end;
        _NotMinifiable ->
            minify_files(Module, Extension, Files, [File0 | Acc])
    end.

concatenate_files([], _Output) ->
    ok;
concatenate_files(Files, Output) ->
    case file:open(Output, [append]) of
        {ok, OutputFile} -> concatenate_files_(Files, OutputFile);
        Error            -> Error
    end.

concatenate_files_([], OutputFile) ->
    file:close(OutputFile);
concatenate_files_([File | Files], OutputFile) ->
    case file:read_file(File) of
        {ok, Data} ->
            case file:write(OutputFile, Data) of
                ok     -> concatenate_files_(Files, OutputFile);
                _Error -> {error, <<"Could not concatenate file ", File/binary>>}
            end;
        Error ->
            Error
    end.

gzip_all_files(Directory) ->
    Files = filelib:wildcard(unicode:characters_to_list(Directory) ++ "/**"),
    gzip_files([unicode:characters_to_binary(File) || File <- Files]).

gzip_files([]) ->
    ok;
gzip_files([File | Files]) ->
    case filelib:is_regular(File) of
        true ->
            case filename:extension(File) of
                <<".gz">> ->
                    gzip_files(Files);
                _ ->
                    case ds_api_util:gzip(File) of
                         ok     -> gzip_files(Files);
                         _Error -> {error, <<"Could not gzip ", File/binary>>}
                    end
            end;
        false ->
            gzip_files(Files)
    end.

fetch_remote_files(RemoteFiles, TmpDir) ->
    fetch_remote_files(RemoteFiles, TmpDir, []).

fetch_remote_files([], _TmpDir, Acc) ->
    {ok, lists:reverse(Acc)};
fetch_remote_files([File | Files], TmpDir, Acc) ->
    case ds_api_util:is_remote_file(File) of
        true ->
            case fetch_remote_file(File, TmpDir) of
                {ok, LocalFile} -> fetch_remote_files(Files, TmpDir, [LocalFile | Acc]);
                Error           -> Error
            end;
        false ->
            fetch_remote_files(Files, TmpDir, [File | Acc])
    end.

fetch_remote_file(RemoteFile, TmpDir) ->
    LocalFile = ds_api_component:file_path(RemoteFile, TmpDir),
    LocalFileStr = unicode:characters_to_list(LocalFile),
    case filelib:ensure_dir(LocalFileStr) of
        ok ->
            RemoteFileStr = unicode:characters_to_list(RemoteFile),
            HTTPOptions = [{timeout, 30000}],
            Options = [{stream, LocalFileStr}],
            case httpc:request(get, {RemoteFileStr, []}, HTTPOptions, Options) of
                {ok, saved_to_file} ->
                    ok;
                _Error ->
                    {error, <<"Could not fetch remote file from ", RemoteFile/binary>>}
            end;
        Error ->
            Error
    end.

check_diversity_json(_Component, _Version, _Diversity) ->
    ok.

write_compact_diversity_json(Path, Diversity) ->
    try jiffy:encode(Diversity) of
        Data -> file:write_file(Path, Data)
    catch
        error:Error -> {error, Error}
    end.
