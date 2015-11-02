-module(ds_api_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Procs = [{ds_api_cache, {ds_api_cache, start_link, []},
              permanent, 5000, worker, [ds_api_cache]}],
    {ok, {{one_for_one, 10, 10}, Procs}}.
