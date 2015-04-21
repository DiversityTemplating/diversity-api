-module(divapi_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

-export([is_production/0]).

start(_Type, _Args) ->
    inets:start(),
    Routes = [{"/", diversity_api_handler, []},
              {"/minify-js", components_js_minifier, []},
              {"/components/", component_list_handler, []},
              {"/components/:component/", component_handler, []},
              {"/components/:component/register", component_action, [register]},
              {"/components/:component/update", component_action, [update]},
              {"/components/:component/[...]", component_handler, []}],
    Dispatch = cowboy_router:compile([
        {'_', Routes}
    ]),
    {ok, Port} = application:get_env(divapi, port),
    cowboy:start_http(diversity_api_listener, 100, [{port, Port}], [
        {compress, true},
        {env, [{dispatch, Dispatch}]}
    ]),
    %% Get all nodes
    {ok, ListOfNodes} = application:get_env(divapi, nodes),
    %% Connect to all nodes.
    [net_kernel:connect_node(Node) || Node <- ListOfNodes],
    divapi_sup:start_link().

stop(_State) ->
    ok.

is_production() ->
    %% Decide what kind of mode we are in.
    case application:get_env(divapi, stage) of
        undefined -> true;
        {ok, _ProdEnv} -> false
    end.
