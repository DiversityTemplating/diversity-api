-module(component_list_handler_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0]).

%% API
-export([handle_grouping/1, handle_grouping_nomatch/1, handle_grouping_nomatch2/1]).

all() -> [handle_grouping, handle_grouping_nomatch, handle_grouping_nomatch2].

-record(state, {}).

handle_grouping(_Config) ->
    ok.
    % SimplifiedReq = {http_req,<<"GET">>,'HTTP/1.1'},
    % State = #state{},
    % application:set_env(divapi, repo_dir, "gitdir"),
    %
    % meck:new(cowboy_req),
    % meck:expect(cowboy_req, reply, fun(200, _Headers, Body, _Req) -> {ok, Body} end),
    % meck:expect(cowboy_req, path, fun(Req) -> {<<"/components/">>,Req} end),
    % meck:expect(cowboy_req, qs_val, fun(<<"grouping">>, Req) -> {<<"sidebar">>, Req} end),
    %
    % meck:new(gitlab_utils),
    %
    % PublicProjects = maps:from_list([{<<"component">>, <<"url">>}]),
    % meck:expect(gitlab_utils, get_public_projects, fun() -> PublicProjects end),
    %
    % meck:new(git_utils),
    % GroupingJson = <<"{\"grouping\":[\"sidebar\",\"othergroup\"]}">>,
    % meck:expect(git_utils, get_diversity_json, fun(<<"component">>,_,_) -> GroupingJson end),
    %
    % Expected = <<"[\"component\"]">>,
    % Response = component_list_handler:handle(SimplifiedReq, State),
    %
    % Response = {ok, Expected, State},
    % meck:unload(cowboy_req),
    % meck:unload(gitlab_utils),
    % meck:unload(git_utils).

handle_grouping_nomatch(_Config) ->
    ok.
    % SimplifiedReq = {http_req,<<"GET">>,'HTTP/1.1'},
    % State = #state{},
    % application:set_env(divapi, repo_dir, "gitdir"),
    %
    % meck:new(cowboy_req),
    % meck:expect(cowboy_req, reply, fun(200, _Headers, Body, _Req) -> {ok, Body} end),
    % meck:expect(cowboy_req, path, fun(Req) -> {<<"/components/">>,Req} end),
    % meck:expect(cowboy_req, qs_val, fun(<<"grouping">>, Req) -> {<<"nonexisting">>, Req} end),
    %
    % meck:new(gitlab_utils),
    %
    % PublicProjects = maps:from_list([{<<"component">>, <<"url">>}]),
    % meck:expect(gitlab_utils, get_public_projects, fun() -> PublicProjects end),
    %
    % meck:new(git_utils),
    % GroupingJson = <<"{\"grouping\":[\"sidebar\",\"othergroup\"]}">>,
    % meck:expect(git_utils, get_diversity_json, fun(<<"component">>,_,_) -> GroupingJson end),
    %
    % Expected = <<"[]">>,
    % Response = component_list_handler:handle(SimplifiedReq, State),
    %
    % Response = {ok, Expected, State},
    % meck:unload(cowboy_req),
    % meck:unload(gitlab_utils),
    % meck:unload(git_utils).

handle_grouping_nomatch2(_Config) ->
    ok.
    % SimplifiedReq = {http_req,<<"GET">>,'HTTP/1.1'},
    % State = #state{},
    % application:set_env(divapi, repo_dir, "gitdir"),
    %
    % meck:new(cowboy_req),
    % meck:expect(cowboy_req, reply, fun(200, _Headers, Body, _Req) -> {ok, Body} end),
    % meck:expect(cowboy_req, path, fun(Req) -> {<<"/components/">>,Req} end),
    % meck:expect(cowboy_req, qs_val, fun(<<"grouping">>, Req) -> {<<"sidebar">>, Req} end),
    %
    % meck:new(gitlab_utils),
    %
    % PublicProjects = maps:from_list([{<<"component">>, <<"url">>}]),
    % meck:expect(gitlab_utils, get_public_projects, fun() -> PublicProjects end),
    %
    % meck:new(git_utils),
    % GroupingJson = <<"{\"grouping\":[\"othergroup\"]}">>,
    % meck:expect(git_utils, get_diversity_json, fun(<<"component">>,_,_) -> GroupingJson end),
    %
    % Expected = <<"[]">>,
    % Response = component_list_handler:handle(SimplifiedReq, State),
    %
    % Response = {ok, Expected, State},
    % meck:unload(cowboy_req),
    % meck:unload(gitlab_utils),
    % meck:unload(git_utils).
