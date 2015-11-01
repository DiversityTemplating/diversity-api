-module(ds_api_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

-define(DEFAULT_PORT, 80).
-define(DEFAULT_LISTENER_COUNT, 100).

start(_Type, _Args) ->
    %% Expand components directory to an absolute path and create it
    ComponentsDir0 = ds_api:components_dir(),
    ComponentsDir1 = filename:absname(ComponentsDir0),
    ok = ds_api:set_config(components_dir, ComponentsDir1),
    ok = filelib:ensure_dir(ComponentsDir1),

    %% Setup cowboy routes
    ComponentConstraintFun = fun (<<>>)      -> false;
                                 (Component) -> {true, Component}
                             end,
    Constraints0 = [{component, function, ComponentConstraintFun}],
    VersionConstraintFun = fun (Value) ->
                                   case ds_api_version:to_version(Value) of
                                       {ok, Version}   -> {true, Version};
                                       {error, badarg} -> false
                                   end
                           end,
    Constraints1 = [{version, function, VersionConstraintFun} | Constraints0],

    Routes = [
        {"/components",                                         ds_api_components_handler, []},
        {"/components/:component",                Constraints0, ds_api_component_handler, []},
        {"/components/:component/:version/[...]", Constraints1, ds_api_version_handler, []}
    ],
    HostMatch = ds_api:config(host_match, '_'),
    Dispatch = cowboy_router:compile([{HostMatch, Routes}]),

    %% Start cowboy
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

