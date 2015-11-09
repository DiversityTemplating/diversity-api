-module(ds_api_stage).

-export([component_dir/2]).

-export([init_version_dir/2]).
-export([init_version_dir/3]).

-export([stream_remote_file/4]).
-export([get_remote_file_info/1]).
-export([flush_stream/1]).
-export([send_stream/3]).

component_dir(Component, Stage) ->
    ComponentsDir = ds_api:components_dir(),
    filename:join([ComponentsDir, Stage, <<Component/binary, ".git">>]).

init_version_dir(Component, Stage) ->
    init_version_dir_(Component, Stage, ds_api_util:tmp_path()).

init_version_dir(Component, Stage, TmpDir) ->
    init_version_dir_(Component, Stage, filename:join(TmpDir, Component)).

init_version_dir_(Component, Stage, TmpDir) ->
    ok = filelib:ensure_dir(TmpDir),
    ok = file:make_dir(TmpDir),
    ok = ds_api_component:make_version_dir(TmpDir),
    FilesDir = ds_api_component:files_dir(TmpDir),
    VersionDir = component_dir(Component, Stage),
    DiversityPath = filename:join(VersionDir, <<"diversity.json">>),
    try
        {ok, #{<<"version">> := VersionBin}} = ds_api_util:read_json_file(DiversityPath),
        Version = ds_api_version:to_version(VersionBin),
        ok = link_stage_version(VersionDir, FilesDir),
        ok = ds_api_preprocess:run(Component, Version, TmpDir),
        TmpDir
    catch
        error:_ ->
            ds_api_util:delete_dir(TmpDir),
            undefined
    end.

link_stage_version(VersionDir, FilesDir) ->
    VersionDirSize = byte_size(VersionDir),
    LinkFile = fun (File, ok) ->
                       <<$/, Path/binary>> = binary_part(File, VersionDirSize, byte_size(File) - VersionDirSize),
                       Destination = filename:join(FilesDir, Path),
                       ok = filelib:ensure_dir(Destination),
                       ok = file:make_symlink(File, Destination)
               end,
    filelib:fold_files(VersionDir, <<"^(?!\.git$)">>, true, LinkFile, ok).

stream_remote_file(Component, Version, GZip, PathAndQuery) ->
    FallbackAPI = ds_api:fallback_api(),
    VersionBin = ds_api_version:to_binary(Version),
    FallbackFile = <<FallbackAPI/binary, "/components",
                     $/, Component/binary,
                     $/, VersionBin/binary,
                     $/, PathAndQuery/binary>>,
    FallbackHeaders = case GZip of
                          true  -> [{"Accept-Encoding", "gzip"}];
                          false -> []
                      end,
    FallbackRequest = {unicode:characters_to_list(FallbackFile), FallbackHeaders},
    Options = [{sync, false}, {stream, self}],
    httpc:request(get, FallbackRequest, [], Options).

get_remote_file_info(RequestId) ->
    receive
        {http, {RequestId, stream_start, Headers}} ->
            get_info(Headers)
    after 1000 ->
              flush_stream(RequestId),
              {undefined, undefined}
    end.

send_stream(Transport, Socket, RequestId) ->
    receive
        {http, {RequestId, stream, BodyPart}} ->
            case Transport:send(Socket, BodyPart) of
                ok ->
                    send_stream(Socket, Transport, RequestId);
                Error ->
                    flush_stream(RequestId),
                    Error
            end;
        {http, {RequestId, stream_end, _Headers}} ->
            ok
    after 1000 ->
              flush_stream(RequestId),
              {error, timeout}
    end.

flush_stream(RequestId) ->
    ok = httpc:cancel_request(RequestId),
    flush_stream_(RequestId).

flush_stream_(RequestId) ->
    receive
        {http, {RequestId, _, _}} -> flush_stream_(RequestId)
    after 0 -> ok
    end.

get_info(Headers) ->
    case {parse_content_length(Headers), parse_last_modified(Headers)} of
        {Size, Modified} when Size =:= undefined; Modified =:= undefined ->
            {undefined, undefined};
        {Size, Modified} ->
            {Size, Modified}
    end.

parse_content_length(Headers) ->
    try
        ContentLength = proplists:get_value("content-length", Headers),
        cow_http_hd:parse_content_length(unicode:characters_to_binary(ContentLength))
    catch
        error:_ -> undefined
    end.

parse_last_modified(Headers) ->
    try
        LastModified = proplists:get_value("last-modified", Headers),
        cow_http_hd:parse_last_modified(unicode:characters_to_binary(LastModified))
    catch
        error:_ -> undefined
    end.
