-module(ds_api_components_handler).

-export([init/2]).
-export([content_types_provided/2]).
-export([to_json/2]).

init(Req0, []) ->
    Req1 = ds_api_util:set_access_control_headers(Req0),
    {cowboy_rest, Req1, no_state}.

content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, to_json}], Req, State}.

to_json(Req, State) ->
    {jiffy:encode(ds_api_component:components()), Req, State}.
