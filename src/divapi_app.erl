-module(divapi_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).


start(_Type, _Args) ->
    inets:start(),
    Routes = [{"/", diversity_api_handler, []},
              {"/components/", component_list_handler, []},
              {"/components/:component/", component_handler, []},
              {"/components/:component/register", component_action, [register]},
              {"/components/:component/update", component_action, [update]},
              {"/components/:component/[...]", component_handler, []}],
    Dispatch = cowboy_router:compile([
        {'_', Routes}
    ]),
    {ok, Port} = application:get_env(divapi, port),
    cowboy:start_http(diversity_api_listener, 100, [{port, Port}],
        [{env, [{dispatch, Dispatch}]}]
    ),
    connect_to_swarm(),
	divapi_sup:start_link().

stop(_State) ->
	ok.

connect_to_swarm() ->
    {ok, ListOfNodes} = application:get_env(divapi, nodes),
    lists:foreach(
        fun (NodeName) ->
            Res = net_kernel:connect_node(NodeName),
            io:format("~n~p~p~n", [NodeName, Res]),
            ok
        end,
        ListOfNodes
    ),
    ok.
