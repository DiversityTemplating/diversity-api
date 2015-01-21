-module(divapi_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Procs = [{divapi_cache, {divapi_cache, start_link, []},
              permanent, 5000, worker, [divapi_cache]}],
    {ok, {{one_for_one, 10, 10}, Procs}}.
