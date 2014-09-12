-module(divapi_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

-define(PORT, 8181).

start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([
        {'_', [{"/", diversity_api_handler, []}]}
    ]),
    cowboy:start_http(diversity_api_listener, 100, [{port, ?PORT}],
        [{env, [{dispatch, Dispatch}]}]
    ),
	divapi_sup:start_link().

stop(_State) ->
	ok.
