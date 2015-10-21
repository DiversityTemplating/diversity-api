-module(ds_api_component_update_mgr).

-behaviour(gen_leader).

-export([start_link/0]).

-export([init/1]).
-export([elected/3]).
-export([surrendered/3]).
-export([handle_leader_call/4]).
-export([handle_leader_cast/3]).
-export([from_leader/3]).
-export([handle_call/4]).
-export([handle_cast/3]).
-export([handle_DOWN/3]).
-export([handle_info/3]).
-export([code_change/4]).
-export([terminate/2]).

-define(TAB, ?MODULE).
-record(state, {}).

start_link() ->
    Nodes = ds_api:config(nodes, [node() | nodes()]),
    gen_leader:start_link(?MODULE, Nodes, [], ?MODULE, [], []).

init(_Config) ->
    {ok, #state{}}.

%% Run in leader
elected(State, _Election, undefined) ->
    {ok, ok, State};
%% Run on new candidate that joins
elected(State, _Election, _Leader) ->
    {ok, ok, State}.

%% This code will be run by all losers
surrendered(State, _Synch, _Election) ->
    {ok, State}.

handle_leader_call(_Request, _From, State, _Election) ->
    {reply, ignored, State}.

handle_leader_cast(_Request, State, _Election) ->
    {noreply, State}.

from_leader(_Synch, State, _Election) ->
    {ok, State}.

handle_DOWN(_Node, State, _Election) ->
    {ok, State}.

handle_call(_Msg, _From, State, _Election) ->
    {reply, ignored, State}.

handle_cast(_Msg, State, _Election) ->
    {noreply, State}.

handle_info(_Msg, State, _Election) ->
    {noreply, State}.

code_change(_OldVsn, State, _Election, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
