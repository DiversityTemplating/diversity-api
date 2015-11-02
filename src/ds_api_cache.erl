-module(divapi_cache).

-behaviour(gen_server).

-export([start_link/0]).
-export([get/3]).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-define(CACHE, ?MODULE).

-record(entry, {
          key   :: term(),
          value :: binary(),
          ts    :: timeout(),
          timer :: reference()
         }).

%% @doc Get a value from the cache if it exists, otherwise compute it
%% and send the value to the cache.
get(_Key, Fun, 0) ->
    Fun();
get(Key, Fun, Timeout) ->
    case ets:lookup(?CACHE, Key) of
        %% The file does not exist in the cache
        [] ->
            Value = Fun(),
            gen_server:cast(?CACHE, {put, Key, Value, Timeout}),
            Value;
        %% Exists in cache
        %% Reset the files credits to it's file size
        [Entry] ->
            Entry#entry.value
    end.

start_link() ->
    gen_server:start_link({local, ?CACHE}, ?MODULE, [], []).

init([]) ->
    ?CACHE = ets:new(?CACHE, [named_table, {read_concurrency, true}, {keypos, #entry.key}]),
    {ok, no_state}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({put, Key, Value, Timeout}, State) ->
    %% NOTE: Do janitorization if ETS-table is full

    %% Cancel old timer if Key existed beforehand
    case ets:lookup(?CACHE, Key) of
        []      -> ok;
        [Entry] -> erlang:cancel_timer(Entry#entry.timer)
    end,

    %% Construct and insert the new entry
    NewEntry = #entry{
        key   = Key,
        value = Value,
        ts    = os:timestamp(),
        timer = erlang:send_after(Timeout, self(), {clear, Key})
    },
    ets:insert(?CACHE, [NewEntry]),
    {noreply, State}.

handle_info({clear, Key}, State) ->
    ets:delete(Key),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
