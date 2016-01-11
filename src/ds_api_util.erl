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
-export([multicall/3]).

-include_lib("kernel/include/file.hrl").

%% @doc Run a given command as a port
-spec cmd(binary(), list(term())) -> {ok, binary()} | {error, binary()}.
cmd(Command, Opts) ->
    PortOpts = Opts ++ [stream, exit_status, binary, use_stdio, stderr_to_stdout],
    Port = erlang:open_port({spawn_executable, Command}, PortOpts),
    wait_for_reply(Port).

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

%% @doc Retrive a temporary path
-spec tmp_path() -> binary().
tmp_path() ->
    TmpName = integer_to_binary(erlang:phash2(make_ref())),
    TmpPath = filename:join(ds_api:tmp_dir(), TmpName),
    case filelib:is_file(TmpPath) of
        true  -> tmp_path();
        false -> TmpPath
    end.

%% @doc Recursively delete a directory
-spec delete_dir(binary()) -> ok.
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

%% @doc Given an extension return the files minified name
-spec minified_name(binary(), binary()) -> binary().
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

%% @doc GZip a given file and adjust the modified time to be that of the original file
-spec gzip(binary()) -> ok | {error, term()}.
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

%% @doc Hash and return the hash as a hexbin
-spec hash(binary()) -> binary().
hash(Data) ->
    Hash = crypto:hash(sha256, Data),
    << <<Y>> || <<X:4>> <= Hash, Y <- integer_to_list(X,16)>>.

%% @doc Concatenate the given files into the given output file.
%% The concatenated file will have a modified time of the last modified files which were %% concatenated.
-spec concatenate_files(binary(), binary()) -> ok | {error, term()}.
concatenate_files(Files, Output) ->
    concatenate_files(Files, <<>>, Output).

%% @doc Concatenate the given files into the given output file with delimiter inserted after each file.
%% The concatenated file will have a modified time of the last modified files which were %% concatenated.
-spec concatenate_files(binary(), binary(), binary()) -> ok | {error, term()}.
concatenate_files([], _Delimiter, _Output) ->
    ok;
concatenate_files(Files, Delimiter, Output) ->
    case file:open(Output, [append]) of
        {ok, OutputFile} ->
            Modified0 = {{0, 0, 0}, {0, 0, 0}},
            case concatenate_files_(Files, Modified0, Delimiter, OutputFile) of
                {ok, LastModified} ->
                    file:change_time(Output, LastModified);
                Error ->
                    file:delete(Output),
                    Error
            end;
        Error ->
            file:delete(Output),
            Error
    end.

concatenate_files_([], Modified, _Delimiter, OutputFile) ->
    file:close(OutputFile),
    {ok, Modified};
concatenate_files_([File | Files], Modified0, Delimiter, OutputFile) ->
    case file:read_file(File) of
        {ok, Data} ->
            {_Size, Modified} = get_file_info(File),
            Modified1 = case Modified > Modified0 of
                            true  -> Modified;
                            false -> Modified0
                        end,
            case file:write(OutputFile, Data) of
                ok ->
                    ok = file:write(OutputFile, Delimiter),
                    concatenate_files_(Files, Modified1, Delimiter, OutputFile);
                _Error ->
                    {error, <<"Could not concatenate file ", File/binary>>}
            end;
        Error ->
            Error
    end.

%% @doc Retrive the size and modification time for given file
-spec get_file_info(binary() | undefined) -> {non_neg_integer() | undefined, calendar:datetime() | undefined}.
get_file_info(undefined) ->
    {undefined, undefined};
get_file_info(File) ->
    case file:read_file_info(File, [{time, universal}]) of
        {ok, #file_info{size = Size, mtime = Modified}} -> {Size, Modified};
        _Error                                          -> {undefined, undefined}
    end.

%% @doc Given a request check if we should gzip the response
-spec should_gzip(cowboy_req:req()) -> boolean().
should_gzip(Req) ->
    AcceptEncoding = cowboy_req:parse_header(<<"accept-encoding">>, Req, []),
    proplists:is_defined(<<"gzip">>, AcceptEncoding).

%% @doc Return the gzipped filename if we should gzip it
-spec maybe_gzipped(binary() | undefined, boolean()) -> binary() | undefined.
maybe_gzipped(undefined, _GZip) -> undefined;
maybe_gzipped(File, true)       -> <<File/binary, ".gz">>;
maybe_gzipped(File, false)      -> File.

%% @doc Read a JSON file and decode it
-spec read_json_file(binary()) -> {ok, jiffy:json_value()} | {error, term()}.
read_json_file(JSONFile) ->
    try
        {ok, JSON} = file:read_file(JSONFile),
        {ok, jiffy:decode(JSON, [return_maps])}
    catch
        error:Error -> {error, Error}
    end.

%% @doc Write JSON to file
-spec write_json_file(binary(), jiffy:json_value()) -> ok | {error, term()}.
write_json_file(JSONFile, JSON) ->
    try jiffy:encode(JSON) of
        Binary -> file:write_file(JSONFile, Binary)
    catch
        error:Error -> {error, Error}
    end.

%% @doc Set access control header for a request
-spec set_access_control_headers(cowboy_req:req()) -> cowboy_req:req().
set_access_control_headers(Req) ->
    cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<"*">>, Req).

%% @doc Run a function on all API nodes
-spec multicall(atom(), atom(), list(term())) ->
    {ok, list(term())} | {error, {not_online, list(atom())}}.
multicall(Module, Function, Args) ->
    case rpc:multicall(ds_api:nodes(), Module, Function, Args, 30000) of
        {Results, []}          -> {ok, Results};
        {_Results, ErrorNodes} -> {error, {not_online, ErrorNodes}}
    end.
