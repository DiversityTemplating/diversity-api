-module(ds_api_util).

-export([cmd/2]).
-export([delete_dir/1]).

cmd(Command, WorkingDir) ->
    lager:debug(
      "RUNNING COMMAND~n"
      "COMMAND: ~p~n"
      "CWD: ~p~n",
      [Command, WorkingDir]
     ),
    PortOpts = [exit_status, {cd, WorkingDir}, binary, stderr_to_stdout],
    Port = erlang:open_port({spawn, Command}, PortOpts),
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

delete_dir(Directory) ->
    lists:foreach(
      fun(D) -> ok = file:del_dir(D) end,
      delete_all_files([Directory], [])
     ).

delete_all_files([], EmptyDirs) ->
    EmptyDirs;
delete_all_files([Directory | Rest0], EmptyDirectories) ->
    {ok, FilesInDirectory} = file:list_dir(Directory),
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
    delete_all_files(Rest1, [Directory | EmptyDirectories]).
