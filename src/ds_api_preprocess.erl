-module(ds_api_preprocess).

-export([run/3]).

run(Component, Version, VersionDir) ->
    lager:debug(
      "PREPROCESSING COMPONENT~n"
      "Component: ~p~n"
      "Version: ~p~n"
      "Directory: ~p~n",
      [Component, ds_api_version:to_binary(Version), VersionDir]
     ),

    FilesDir = ds_api_component:files_dir(VersionDir),
    case get_diversity_json(FilesDir) of
        {ok, Diversity} ->
            case check_diversity_json(Component, Version, Diversity, FilesDir) of
                ok ->
                    case write_compact_diversity_json(FilesDir, Diversity) of
                        ok    -> pre_process_files(Diversity, VersionDir);
                        Error -> Error
                    end;
                Error ->
                    Error
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
    lager:debug(
      "PREPROCESSING CSS~n"
      "FILES: ~p~n",
      [MaybeRemoteCSSFiles]
     ),
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
    lager:debug(
      "PREPROCESSING JS~n"
      "FILES: ~p~n",
      [MaybeRemoteJSFiles]
     ),
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

concatenate_files([], _Output) ->
    ok;
concatenate_files(Files, Output) ->
    lager:debug(
      "CONCATENATING FILES~n"
      "OUTPUT: ~p~n"
      "FILES: ~p~n",
      [Output, Files]
     ),
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

gzip_all_files(Directory0) ->
    Directory1 = unicode:characters_to_list(Directory0),
    Files0 = filelib:wildcard("**", Directory1),
    Files1 = [unicode:characters_to_binary(File) || File <- Files0],
    lager:debug(
      "GZIPPING FILES~n"
      "FILES: ~p~n",
      [Files1]
     ),
    gzip_files(Files1).

gzip_files([]) ->
    ok;
gzip_files([File | Files]) ->
    case filelib:is_regular(File) of
        true ->
            case filename:extension(File) of
                <<".gz">> ->
                    gzip_files(Files);
                _ ->
                    CompressedFile = <<File/binary, ".gz">>,
                    case gzip_file(File, CompressedFile) of
                         ok    -> gzip_files(Files);
                         Error -> Error
                    end
            end;
        false ->
            gzip_files(Files)
    end.

gzip_file(Input, Output) ->
    case file:read_file(Input) of
        {ok, Data} -> file:write_file(Output, zlib:gzip(Data));
        Error      -> Error
    end.

get_diversity_json(FilesDir) ->
    try
        DiversityFile = filename:join(FilesDir, <<"diversity.json">>),
        {ok, DiversityBin} = file:read_file(DiversityFile),
        {ok, jiffy:decode(DiversityBin, [return_maps])}
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
                      FilesDir = ds_api_component:files_dir(VersionDir),
                      File1 = filename:join(FilesDir, File0),
                      File2 = unicode:characters_to_list(File1),
                      [unicode:characters_to_binary(File) || File <- filelib:wildcard(File2)]
              end
      end,
      Files
     ).

process_remote_file_name(<<"//", URL/binary>> = Remote, VersionDir) ->
    RemoteDir = ds_api_component:remote_dir(VersionDir),
    Local = filename:join(RemoteDir, URL),
    {ok, <<"http:", Remote/binary>>, Local};
process_remote_file_name(<<"http://", URL/binary>> = Remote, VersionDir) ->
    RemoteDir = ds_api_component:remote_dir(VersionDir),
    Local = filename:join(RemoteDir, URL),
    {ok, Remote, Local};
process_remote_file_name(<<"https://", URL/binary>> = Remote, VersionDir) ->
    RemoteDir = ds_api_component:remote_dir(VersionDir),
    Local = filename:join(RemoteDir, URL),
    {ok, Remote, Local};
process_remote_file_name(_VersionDir, _Local) ->
    false.

%% TODO: Make request async and fire them all off and wait for responses
fetch_remote_files(RemoteFiles) ->
    fetch_remote_files(RemoteFiles, []).

fetch_remote_files([], Acc) ->
    {ok, lists:reverse(Acc)};
fetch_remote_files([{remote, Remote, Local} | Files], Acc) ->
    lager:debug(
      "FETCHING REMOTE FILE~n"
      "URL: ~p~n"
      "FILE: ~p~n",
      [Remote, Local]
     ),
    case fetch_remote_file(Remote, Local) of
        ok    -> fetch_remote_files(Files, [Local | Acc]);
        Error -> Error
    end;
fetch_remote_files([Local | Files], Acc) ->
    fetch_remote_files(Files, [Local | Acc]).

fetch_remote_file(Remote, Local) ->
    File = unicode:characters_to_list(Local),
    case filelib:ensure_dir(File) of
        ok ->
            URL = unicode:characters_to_list(Remote),
            HTTPOptions = [{timeout, 30000}],
            Options = [{stream, File}],
            case httpc:request(get, {URL, []}, HTTPOptions, Options) of
                {ok, saved_to_file} -> ok;
                Error               -> Error
            end;
        Error ->
            Error
    end.

check_diversity_json(_Component, _Version, _Diversity, _FilesDir) ->
    ok.

write_compact_diversity_json(FilesDir, Diversity) ->
    DiversityPath = filename:join(FilesDir, <<"diversity.json">>),
    try jiffy:encode(Diversity) of
        Data -> file:write_file(DiversityPath, Data)
    catch
        error:Error -> {error, Error}
    end.
