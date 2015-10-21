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
    Constraints0 = [{component, nonempty}],
    Constraints1 = [{tag, nonempty} | Constraints0],
    Constraints2 = [{file, nonempty} | Constraints1],

    %% TODO:
    %% Remove settings/settingsForm (better to use diversity.json directly
    %% Let script.min.js be an ordinary file
    %% Is thumbnail needed?
    Routes = [
        {"/components",                                             ds_api_handler,           [list]},
        {"/components/:component",                    Constraints0, ds_api_handler,           [component]},
        {"/components/:component/update",             Constraints0, ds_api_handler,           [update]},
        {"/components/:component/:tag",               Constraints1, ds_api_component_handler, [diversityJSON]},
        {"/components/:component/:tag/settings",      Constraints1, ds_api_component_handler, [settings]},
        {"/components/:component/:tag/settingsForm",  Constraints1, ds_api_component_handler, [settingsForm]},
        {"/components/:component/:tag/files/:file",   Constraints2, ds_api_component_handler, [file]}, 
        {"/components/:component/:tag/script.min.js", Constraints1, ds_api_component_handler, [javascript]},
        {"/components/:component/:tag/css",           Constraints1, ds_api_component_handler, [css]},
        {"/components/:component/:tag/thumbnail",     Constraints1, ds_api_component_handler, [thumbnail]}
%        {"/js",                                       [],           ds_api_js_handler,        []},
%        {"/css",                                      [],           ds_api_css_handler,       []}
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

