-module(components_js_minifier).

-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

%% Cowboy callbacks

init(_, Req, _Opts) ->
    {ok, Req, []}.

handle(Req, State) ->
    {Components, Req1} = cowboy_req:qs_vals(Req),
    {ok, Req2} = cowboy_req:chunked_reply(
        200, [{<<"content-type">>, <<"application/javascript">>}], Req1
    ),
    pmap(
        fun ({Component, Tag}) ->
            Result = divapi_js_minifier:minify(Component, Tag),
            cowboy_req:chunk(Result, Req2),
            Result
        end,
        Components
    ),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.

pmap(Fun, List) ->
    pmap(Fun, List, 5000).

pmap(Fun, List, Timeout) ->
    Parent = self(),

    %% Spawn processes
    MapFun = fun (Item) -> spawn_monitor(fun () -> Parent ! {self(), Fun(Item)} end) end,
    Processes = lists:map(MapFun, List),

    %% Await results
    ReceiveFun = fun ({Pid, Reference}) ->
                         receive
                             {Pid, Result} ->
                                 Result;
                             {'DOWN', Reference, process, Pid, Reason} ->
                                 error({pmap, Pid, Reason})
                         after Timeout ->
                                   error({pmap, Pid, timeout})
                         end
                 end,
    lists:map(ReceiveFun, Processes).
