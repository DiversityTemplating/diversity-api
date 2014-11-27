-module(api_handler_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([basic_handle/1]).

-record(state, {}).

all() -> [basic_handle].

basic_handle(_Config) ->

    SimplifiedReq = {http_req,<<"GET">>,'HTTP/1.1'},
    State = #state{},
    meck:new(cowboy_req),
    meck:expect(cowboy_req, reply, fun(200, _Headers, _Body, Req) -> {ok, Req} end),

    Response = diversity_api_handler:handle(SimplifiedReq, State),
    Response = {ok, SimplifiedReq, State},
    meck:unload(cowboy_req).
