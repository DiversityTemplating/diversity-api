-module(ds_api_util).

-export([cmd/2]).

cmd(Command, WorkingDir) ->
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
