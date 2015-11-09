-module(ds_api_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

-define(DEFAULT_PORT, 80).
-define(DEFAULT_LISTENER_COUNT, 100).

start(_Type, _Args) ->
    %% Expand components directory to an absolute path and create it
    RootDir0 = ds_api:root_dir(),
    RootDir1 = filename:absname(RootDir0),
    ds_api:root_dir(RootDir1),

    filelib:ensure_dir(ds_api:root_dir()),
    file:make_dir(ds_api:root_dir()),
    file:make_dir(ds_api:components_dir()),
    file:make_dir(ds_api:tmp_dir()),

    %% Setup cowboy routes
    Constraints0 = [{component, nonempty}],
    VersionConstraintFun = fun (Value) ->
                                   case ds_api_version:to_version(Value) of
                                       {ok, Version}   -> {true, Version};
                                       {error, badarg} -> false
                                   end
                           end,
    Constraints1 = [{version, VersionConstraintFun} | Constraints0],

    Routes = [
        {"/components",                                         ds_api_components_handler, []},
        {"/components/:component",                Constraints0, ds_api_component_handler, []},
        {"/components/:component/:version/[...]", Constraints1, ds_api_version_handler, []},
        {"/js",                                                 ds_api_concat_handler, [js]},
        {"/css",                                                ds_api_concat_handler, [css]}
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

