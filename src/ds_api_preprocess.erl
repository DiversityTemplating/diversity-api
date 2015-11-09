-module(ds_api_preprocess).

-export([run/3]).
-export([expand_file_names/2]).
-export([fetch_remote_files/2]).
-export([local_file_names/2]).

run(Component, Version, VersionDir) ->
    FilesDir = ds_api_component:files_dir(VersionDir),
    DiversityPath = filename:join(FilesDir, <<"diversity.json">>),
    case ds_api_util:read_json_file(DiversityPath) of
        {ok, Diversity} ->
            case check_diversity_json(Component, Version, Diversity) of
                ok ->
                    ok = ds_api_util:write_json_file(DiversityPath, Diversity),
                    pre_process_files(Diversity, VersionDir);
                Error ->
                    Error
            end;
        _Error ->
            {error, <<"Error when trying to read diversity.json">>}
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

pre_process_css_files(#{<<"style">> := Styles}, VersionDir) ->
    MaybeRemoteCSSFiles = expand_file_names(Styles, VersionDir),
    case fetch_remote_files(MaybeRemoteCSSFiles, VersionDir) of
        ok ->
            LocalCSSFiles = local_file_names(MaybeRemoteCSSFiles, VersionDir),
            case minify_files(ds_api_css, <<".css">>, LocalCSSFiles) of
                {ok, _Minified} -> ok;
                Error           -> Error
            end;
        Error ->
            Error
    end;
pre_process_css_files(_Diversity, _VersionDir) ->
    ok.

pre_process_js_files(#{<<"script">> := Scripts}, VersionDir) ->
    MaybeRemoteJSFiles = expand_file_names(Scripts, VersionDir),
    case fetch_remote_files(MaybeRemoteJSFiles, VersionDir) of
        ok ->
            LocalJSFiles = local_file_names(MaybeRemoteJSFiles, VersionDir),
            case minify_files(ds_api_js, <<".js">>, LocalJSFiles) of
                {ok, MinifiedJSFiles} ->
                    OutputFile = filename:join(VersionDir, <<"script.min.js">>),
                    ds_api_util:concatenate_files(MinifiedJSFiles, <<";">>, OutputFile);
                Error ->
                    Error
            end;
        Error ->
            Error
    end;
pre_process_js_files(_Diversity, _VersionDir) ->
    ok.

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
                            {_Size, Modified} = ds_api_util:get_file_info(File0),
                            ok = file:change_time(Minified, Modified),
                            minify_files(Module, Extension, Files, [Minified | Acc]);
                        Error ->
                            Error
                    end
            end;
        _NotMinifiable ->
            minify_files(Module, Extension, Files, [File0 | Acc])
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

expand_file_names(File, VersionDir) when is_binary(File) ->
    expand_file_names([File], VersionDir);
expand_file_names(Files, VersionDir) when is_list(Files) ->
    lists:flatmap(
      fun (<<"//", _/binary>> = Remote) ->
              [<<"http:", Remote/binary>>];
          (<<"http://", _/binary>> = Remote) ->
              [Remote];
          (<<"https://", _/binary>> = Remote) ->
              [Remote];
          (MaybeWildcard) ->
              FilesDir = ds_api_component:files_dir(VersionDir),
              Wildcard = unicode:characters_to_list(filename:join(FilesDir, MaybeWildcard)),
              [unicode:characters_to_binary(File) || File <- filelib:wildcard(Wildcard)]
      end,
      Files
     ).

local_file_names(Files, VersionDir) ->
    [local_file_name(File, VersionDir) || File <- Files].

local_file_name(File, VersionDir) ->
    case is_remote_file(File) of
        true  ->
            RemoteDir = ds_api_component:remote_dir(VersionDir),
            filename:join(RemoteDir, local_file_path(File));
        false ->
            File
    end.

fetch_remote_files([], _VersionDir) ->
    ok;
fetch_remote_files([File | Files], VersionDir) ->
    case is_remote_file(File) of
        true ->
            case fetch_remote_file(File, VersionDir) of
                ok    -> fetch_remote_files(Files, VersionDir);
                Error -> Error
            end;
        false ->
            fetch_remote_files(Files, VersionDir)
    end.

fetch_remote_file(RemoteFile, VersionDir) ->
    LocalFile = local_file_name(RemoteFile, VersionDir),
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

is_remote_file(<<"http://", _/binary>>)  -> true;
is_remote_file(<<"https://", _/binary>>) -> true;
is_remote_file(_Local)                   -> false.

local_file_path(<<"http://", URL/binary>>)  -> filename:join(http, URL);
local_file_path(<<"https://", URL/binary>>) -> filename:join(https, URL).

check_diversity_json(_Component, _Version, _Diversity) -> ok.
