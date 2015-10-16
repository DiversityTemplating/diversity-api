-module(ds_api_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

-define(DEFAULT_PORT, 80).
-define(DEFAULT_LISTENER_COUNT, 100).

start(_Type, _Args) ->
    %% Create components directory if missing
    ok = filelib:ensure_dir(ds_api:components_dir()),

    %% Setup cowboy routes
    Routes = [{"/minify-js", ds_api_components_js_minifier, []},
              {"/components/", ds_api_component_list_handler, []},
              {"/components/:component/", ds_api_component_handler, []},
              {"/components/:component/register", ds_api_component_action, [register]},
              {"/components/:component/update", ds_api_component_action, [update]},
              {"/components/:component/[...]", ds_api_component_handler, []}],
    Dispatch = cowboy_router:compile([{'_', Routes}]),
    Port = ds_api:config(port, ?DEFAULT_PORT),
    ListenerCount = ds_api:config(listener_count, ?DEFAULT_LISTENER_COUNT),
    {ok, _HTTP} = cowboy:start_http(
      ds_api_http,
      ListenerCount,
      [{port, Port}],
      [
       {compress, true},
       {env, [{dispatch, Dispatch}]}
      ]
    ),

    %% Connect to all nodes.
    ListOfNodes = ds_api:config(nodes, []),
    [net_kernel:connect_node(Node) || Node <- ListOfNodes],
    ds_api_sup:start_link().

stop(_State) ->
    ok = cowboy:stop(ds_api_http).

