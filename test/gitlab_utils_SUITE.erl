-module(gitlab_utils_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([get_public_projects/1]).

all() -> [get_public_projects].

get_public_projects(_Config) ->
    inets:start(),
    Res = gitlab_utils:get_public_projects(),
    ct:pal("~p",[Res]).

