-module(ds_api_components_handler).

-export([init/3]).
-export([content_types_provided/2]).
-export([to_json/2]).

init(_Type, Req, []) ->
    {upgrade, protocol, cowboy_rest, Req, no_state}.

content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, to_json}], Req, State}.

to_json(Req, State) ->
    {jiffy:encode(ds_api_component:components()), Req, State}.
