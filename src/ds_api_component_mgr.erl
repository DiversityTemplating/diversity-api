-module(ds_api_component_mgr).

-behaviour(gen_leader).

-export([start_link/0]).
-export([list_components/0]).
-export([list_versions/1]).
-export([exists/1]).
-export([exists/2]).

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
-export([terminate/2]).
-export([code_change/4]).

-define(TAB, ?MODULE).
-record(state, {}).

start_link() ->
    Nodes = ds_api:config(nodes, [node() | nodes()]),
    gen_leader:start_link(?MODULE, Nodes, [], ?MODULE, [], []).

list_components() ->
    [{components, Components}] = ets:lookup(?TAB, components),
    Components.

list_versions(Component) ->
    Key = {component, Component},
    case ets:lookup(?TAB, Key) of
        [{Key, Versions}] -> Versions;
        _Error            -> undefined
    end.

exists(Component) ->
    list_versions(Component) =/= undefined.

exists(Component, Version) ->
    case list_versions(Component) of
        undefined -> false;
        Versions  -> lists:member(Version, Versions)
    end.

init(_Config) ->
    {ok, #state{}}.

%% Run in leader
elected(State, _Election, undefined) ->
    {ok, ok, State};
%% Run on new candidate that joins
elected(State, _Election, Leader) ->
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
