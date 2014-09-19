-module(divapi_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

-define(PORT, 8181).

start(_Type, _Args) ->
    inets:start(),
    Routes = [{"/", diversity_api_handler, []},
              {"/components/", component_list_handler, []},
              {"/components/:component/[...]", component_handler, []}],
    Dispatch = cowboy_router:compile([
        {'_', Routes}
    ]),
    cowboy:start_http(diversity_api_listener, 100, [{port, ?PORT}],
        [{env, [{dispatch, Dispatch}]}]
    ),
    divapi_cache:start_link(),
	divapi_sup:start_link().

stop(_State) ->
	ok.
