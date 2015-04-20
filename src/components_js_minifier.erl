-module(components_js_minifier).

-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_, Req, _Opts) ->
    {ok, Req, []}.

handle(Req, State) ->
    {Components, Req1} = cowboy_req:qs_vals(Req),
    Data = divapi_cache:get(
        {minified_result, Components},
        fun () ->
            Result = pmap(
                fun ({Component, Tag}) ->
                    divapi_js_minifier:minify(Component, Tag)
                end,
                Components
            ),
            iolist_to_binary(Result)
        end,
        1000 * 60 * 60 * 5 % 5 hours
    ),
    {ok, Req2} = cowboy_req:reply(
        200, [{<<"content-type">>, <<"application/javascript">>}], Data, Req1
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
