-module(ds_api_preprocess).

-export([run/3]).

run(Component, Version, VersionDir) ->
    FilesDir = ds_api_component:files_dir(VersionDir, Version),
    case get_diversity_json(FilesDir) of
        {ok, Diversity} ->
            case check_diversity_json(Component, Version, Diversity, FilesDir) of
                ok    -> pre_process_files(Diversity, VersionDir);
                Error -> Error
            end;
        Error ->
            Error
    end.

pre_process_files(Diversity, VersionDir) ->
    case pre_process_css_files(Diversity, VersionDir) of
        ok ->
            case pre_process_js_files(Diversity, VersionDir) of
                ok    -> gzip_all_files(VersionDir);
                Error -> Error
            end;
        Error ->
            Error
    end.

pre_process_css_files(Diversity, VersionDir) ->
    MaybeRemoteCSSFiles = get_diversity_json_files(<<"style">>, Diversity, VersionDir),
    case fetch_remote_files(MaybeRemoteCSSFiles) of
        {ok, LocalCSSFiles} ->
            case minify_files(ds_api_css, <<".css">>, LocalCSSFiles) of
                {ok, _Minified} -> ok;
                Error           -> Error
            end;
        Error ->
            Error
    end.

pre_process_js_files(Diversity, VersionDir) ->
    MaybeRemoteJSFiles = get_diversity_json_files(<<"script">>, Diversity, VersionDir),
    case fetch_remote_files(MaybeRemoteJSFiles) of
        {ok, LocalJSFiles} ->
            case minify_files(ds_api_js, <<".js">>, LocalJSFiles) of
                {ok, MinifiedJSFiles} ->
                    OutputFile = filename:join(VersionDir, <<"script.min.js">>),
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

concatenate_files(Files, Output) ->
    case file:open(Output, [append]) of
        {ok, OutputFile} ->
            concatenate_files_(Files, OutputFile);
        Error ->
            Error
    end.

concatenate_files_([], OutputFile) ->
    file:close(OutputFile);
concatenate_files_([File | Files], OutputFile) ->
    case file:read_file(File) of
        {ok, Data} ->
            case file:write(OutputFile, Data) of
                ok    -> concatenate_files_(Files, OutputFile);
                Error -> Error
            end;
        Error ->
            Error
    end.

gzip_all_files(Directory) ->
    Wildcard0 = filename:join(Directory, <<"**">>),
    Wildcard1 = unicode:characters_to_list(Wildcard0),
    Files = filelib:wildcard(Wildcard1),
    gzip_files(Files).

gzip_files([]) ->
    ok;
gzip_files([File | Files]) ->
    case filename:extension(File) of
        <<".gz">> ->
            gzip_files(Files);
        _ ->
            case file:read_file(File) of
                {ok, Data} ->
                    CompressedData = zlib:gzip(Data),
                    CompressedFile = <<File/binary, ".gz">>,
                    case file:write_file(CompressedFile, CompressedData) of
                        ok    -> gzip_files(Files);
                        Error -> Error
                    end;
                Error ->
                    Error
            end
    end.

get_diversity_json(FilesDir) ->
    try
        DiversityFile = filename:join(FilesDir, <<"diversity.json">>),
        {ok, DiversityBin} = file:read_file(DiversityFile),
        jiffy:decode(DiversityBin)
    catch
        error:Error -> {error, Error}
    end.

get_diversity_json_files(Property, Diversity, VersionDir) ->
    %% Get the property, this is used for both style and script-property
    Files = case maps:find(Property, Diversity) of
                {ok, F} when is_binary(F) -> [F];
                {ok, Fs} when is_list(Fs) -> Fs;
                error                     -> []
            end,

    lists:flatmap(
      fun (File0) ->
              case process_remote_file_name(File0, VersionDir) of
                  %% If the file is a remote file we get a local path
                  %% to it
                  {ok, Remote, Local} ->
                      [{remote, Remote, Local}];
                  %% Otherwise treat it as a wildcard match for local files
                  false ->
                      File1 = unicode:characters_to_list(File0),
                      [unicode:characters_to_binary(File)
                       || File <- filelib:wildcard(File1)]
              end
      end,
      Files
     ).

process_remote_file_name(<<"//", URL/binary>> = Remote, VersionDir) ->
    Local = filename:join(VersionDir, URL),
    {ok, <<"http:", Remote/binary>>, Local};
process_remote_file_name(<<"http://", URL/binary>> = Remote, VersionDir) ->
    Local = filename:join(VersionDir, URL),
    {ok, Remote, Local};
process_remote_file_name(<<"https://", URL/binary>> = Remote, VersionDir) ->
    Local = filename:join(VersionDir, URL),
    {ok, Remote, Local};
process_remote_file_name(_VersionDir, _Local) ->
    false.

%% TODO: Make request async and fire them all off and wait for responses
fetch_remote_files(RemoteFiles) ->
    fetch_remote_files(RemoteFiles, []).

fetch_remote_files([], Acc) ->
    {ok, lists:reverse(Acc)};
fetch_remote_files([{remote, Remote, Local} | Files], Acc) ->
    case fetch_remote_file(Remote, Local) of
        ok    -> fetch_remote_files(Files, [Local | Acc]);
        Error -> Error
    end;
fetch_remote_files([Local | Files], Acc) ->
    fetch_remote_files(Files, [Local | Acc]).

fetch_remote_file(Remote, Local) ->
    HTTPOptions = [{timeout, 30000}],
    Options = [{stream, Local}],
    Request = {unicode:characters_to_list(Remote), []},
    case httpc:request(get, Request, HTTPOptions, Options) of
        {ok, saved_to_file} -> ok;
        Error               -> Error
    end.

check_diversity_json(_Component, _Version, _Diversity, _FilesDir) ->
    ok.
