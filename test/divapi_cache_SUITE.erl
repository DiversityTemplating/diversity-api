-module(divapi_cache_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0]).
%% API
-export([test_empty_cache/1]).

all() -> [test_empty_cache].

test_empty_cache(_Config) ->
ok.
