-module(ds_api_util).

-export([cmd/2]).
-export([tmp_path/0]).
-export([delete_dir/1]).
-export([minified_name/2]).
-export([gzip/1]).
-export([hash/1]).
-export([concatenate_files/2]).
-export([concatenate_files/3]).
-export([get_file_info/1]).
-export([should_gzip/1]).
-export([maybe_gzipped/2]).
-export([read_json_file/1]).
-export([write_json_file/2]).
-export([set_access_control_headers/1]).

-include_lib("kernel/include/file.hrl").

cmd(Command, WorkingDir) ->
    PortOpts = [exit_status, {cd, WorkingDir}, binary, stderr_to_stdout],
    Port = erlang:open_port({spawn, Command}, PortOpts),
    wait_for_reply(Port).

tmp_path() ->
    TmpName = integer_to_binary(erlang:phash2(make_ref())),
    TmpPath = filename:join(ds_api:tmp_dir(), TmpName),
    case filelib:is_file(TmpPath) of
        true  -> tmp_path();
        false -> TmpPath
    end.

%% TODO: More correct timeout (now it's a timeout from last received chunk)
wait_for_reply(Port) ->
    Timeout = 30000,
    wait_for_reply(Port, <<>>, Timeout).

wait_for_reply(Port, Acc, Timeout) ->
    receive
        %% Incoming data, accumulate and wait for rest
        {Port, {data, Chunk}} ->
            wait_for_reply(Port, <<Acc/binary, Chunk/binary>>, Timeout);
        %% Success, return reply
        {Port, {exit_status, 0}} ->
            {ok, Acc};
        %% Something went wrong, return an error with the reply
        {Port, {exit_status, _}} ->
            {error, Acc}
    end.

delete_dir(undefined) ->
    ok;
delete_dir(Directory) ->
    lists:foreach(
      fun(D) -> ok = file:del_dir(D) end,
      delete_all_files([Directory], [])
     ).

delete_all_files([], EmptyDirs) ->
    EmptyDirs;
delete_all_files([Directory | Rest0], EmptyDirectories) ->
    case file:list_dir(Directory) of
        {ok, FilesInDirectory} ->
            {Files, Directories} = lists:foldl(
                                     fun(F, {Fs, Ds}) ->
                                             Path = filename:join(Directory, F),
                                             case filelib:is_dir(Path) of
                                                 true ->
                                                     {Fs, [Path | Ds]};
                                                 false ->
                                                     {[Path | Fs], Ds}
                                             end
                                     end,
                                     {[],[]},
                                     FilesInDirectory
                                    ),
            lists:foreach(fun(F) -> ok = file:delete(F) end, Files),
            Rest1 = Rest0 ++ Directories,
            delete_all_files(Rest1, [Directory | EmptyDirectories]);
        _Error ->
            delete_all_files(Rest0, EmptyDirectories)
    end.

minified_name(File0, Extension) ->
    case filename:extension(File0) of
        Extension ->
            File1 = filename:rootname(File0, Extension),
            case filename:extension(File1) of
                <<".min">> -> File0;
                _Otherwise -> <<File1/binary, ".min", Extension/binary>>
            end;
        _Otherwise ->
            File0
    end.

gzip(Input) ->
    case file:read_file(Input) of
        {ok, Data} ->
            {_Size, Modified} = get_file_info(Input),
            Output = <<Input/binary, ".gz">>,
            case file:write_file(Output, zlib:gzip(Data)) of
                ok    -> file:change_time(Output, Modified);
                Error -> Error
            end;
        Error ->
            Error
    end.

hash(Data) ->
    Hash = crypto:hash(sha256, Data),
    << <<Y>> || <<X:4>> <= Hash, Y <- integer_to_list(X,16)>>.

concatenate_files(Files, Output) ->
    concatenate_files(Files, <<>>, Output).

concatenate_files([], _Delimiter, _Output) ->
    ok;
concatenate_files(Files, Delimiter, Output) ->
    case file:open(Output, [append]) of
        {ok, OutputFile} -> concatenate_files_(Files, Delimiter, OutputFile);
        Error            -> Error
    end.

concatenate_files_([], _Delimiter, OutputFile) ->
    file:close(OutputFile);
concatenate_files_([File | Files], Delimiter, OutputFile) ->
    case file:read_file(File) of
        {ok, Data} ->
            case file:write(OutputFile, Data) of
                ok ->
                    ok = file:write(OutputFile, Delimiter),
                    concatenate_files_(Files, Delimiter, OutputFile);
                _Error ->
                    {error, <<"Could not concatenate file ", File/binary>>}
            end;
        Error ->
            Error
    end.

get_file_info(undefined) ->
    {undefined, undefined};
get_file_info(File) ->
    case file:read_file_info(File) of
        {ok, #file_info{size = Size, mtime = Modified}} -> {Size, Modified};
        _Error                                          -> {undefined, undefined}
    end.

should_gzip(Req) ->
    AcceptEncoding = cowboy_req:parse_header(<<"accept-encoding">>, Req, []),
    proplists:is_defined(<<"gzip">>, AcceptEncoding).

maybe_gzipped(undefined, _GZip) -> undefined;
maybe_gzipped(File, true)       -> <<File/binary, ".gz">>;
maybe_gzipped(File, false)      -> File.

read_json_file(DiversityPath) ->
    try
        {ok, Diversity0} = file:read_file(DiversityPath),
        {ok, jiffy:decode(Diversity0, [return_maps])}
    catch
        error:Error ->
            {error, Error}
    end.

write_json_file(Path, Diversity) ->
    try jiffy:encode(Diversity) of
        Data -> file:write_file(Path, Data)
    catch
        error:Error -> {error, Error}
    end.

set_access_control_headers(Req) ->
    cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<"*">>, Req).
