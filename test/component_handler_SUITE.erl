-module(component_handler_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0]).
%% API
-export([
    handle_component_tags/1,
    handle_component_tag/1,
    handle_component_part_tag/1,
    handle_component_wildcard/1
]).

-record(state, {}).

all() -> [handle_component_tags, handle_component_tag, handle_component_part_tag, handle_component_wildcard].

handle_component_tags(_Config) ->
    SimplifiedReq = {http_req,<<"GET">>,'HTTP/1.1'},
    State = #state{},
    application:set_env(divapi, repo_dir, "gitdir"),
    meck:new(cowboy_req),
    meck:expect(cowboy_req, reply, fun(200, Headers, Body, Req) -> {ok, Body} end),
    meck:expect(cowboy_req, path_info, fun(Req) -> {[],Req} end),
    meck:expect(cowboy_req, binding, fun(component, Req) -> {<<"Component">>, Req} end),

    meck:new(git_utils),
    meck:expect(git_utils, tags, fun(_) -> [<<"0.1.0">>, <<"0.2.0">>] end),

    Expected = jiffy:encode([<<"0.1.0">>, <<"0.2.0">>]),
    Response = component_handler:handle(SimplifiedReq, State),

    Response = {ok, Expected, State},
    meck:unload(cowboy_req),
    meck:unload(git_utils).

handle_component_tag(_Config) ->
    SimplifiedReq = {http_req,<<"GET">>,'HTTP/1.1'},
    State = #state{},
    application:set_env(divapi, repo_dir, "gitdir"),
    meck:new(cowboy_req),
    meck:expect(cowboy_req, reply, fun(200, Headers, Body, Req) -> {ok, Body} end),
    meck:expect(cowboy_req, path_info, fun(Req) -> {[<<"0.1.1">>],Req} end),
    meck:expect(cowboy_req, binding, fun(component, Req) -> {<<"Component">>, Req} end),
    %%git_utils:get_diversity_json
    meck:new(git_utils),
    meck:expect(git_utils, tags, fun(_) -> [<<"0.1.1">>, <<"0.2.0">>] end),
    meck:expect(git_utils, get_diversity_json, fun(<<"Component">>,_) -> "{}" end),

    Expected = "{}",
    Response = component_handler:handle(SimplifiedReq, State),

    Response = {ok, Expected, State},
    meck:unload(cowboy_req),
    meck:unload(git_utils).

handle_component_part_tag(_Config) ->
    SimplifiedReq = {http_req,<<"GET">>,'HTTP/1.1'},
    State = #state{},
    application:set_env(divapi, repo_dir, "gitdir"),
    meck:new(cowboy_req),
    meck:expect(cowboy_req, reply, fun(200, Headers, Body, Req) -> {ok, Body} end),
    meck:expect(cowboy_req, path_info, fun(Req) -> {[<<"0.1">>],Req} end),
    meck:expect(cowboy_req, binding, fun(component, Req) -> {<<"Component">>, Req} end),
    %%git_utils:get_diversity_json
    meck:new(git_utils),
    meck:expect(git_utils, tags, fun(_) -> [<<"0.1.1">>, <<"0.2.0">>] end),
    meck:expect(git_utils, get_diversity_json, fun(<<"Component">>,_) -> "{}" end),

    Expected = "{}",
    Response = component_handler:handle(SimplifiedReq, State),

    Response = {ok, Expected, State},
    meck:unload(cowboy_req),
    meck:unload(git_utils).

handle_component_wildcard(_Config) ->
    SimplifiedReq = {http_req,<<"GET">>,'HTTP/1.1'},
    State = #state{},
    application:set_env(divapi, repo_dir, "gitdir"),
    meck:new(cowboy_req),
    meck:expect(cowboy_req, reply, fun(200, Headers, Body, Req) -> {ok, Body} end),
    meck:expect(cowboy_req, path_info, fun(Req) -> {[<<"*">>],Req} end),
    meck:expect(cowboy_req, binding, fun(component, Req) -> {<<"Component">>, Req} end),
    %%git_utils:get_diversity_json
    meck:new(git_utils),
    meck:expect(git_utils, tags, fun(_) -> [<<"0.1.1">>, <<"0.2.0">>] end),
    meck:expect(git_utils, get_diversity_json, fun(<<"Component">>,_) -> "{}" end),

    Expected = "{}",
    Response = component_handler:handle(SimplifiedReq, State),

    Response = {ok, Expected, State},
    meck:unload(cowboy_req),
    meck:unload(git_utils).