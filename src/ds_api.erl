-module(ds_api).

-define(DEFAULT_COMPONENTS_DIR, "/tmp/ds_api/components").

-export([accepting/0]).
-export([is_production/0]).
-export([components_dir/0]).
-export([config/1]).
-export([config/2]).

accepting() ->
    config(accepting, false).

is_production() ->
    config(stage) =/= undefined.

components_dir() ->
    case config(components_dir, ?DEFAULT_COMPONENTS_DIR) of
        Dir when is_binary(Dir) -> Dir;
        Dir when is_list(Dir)   -> unicode:characters_to_binary(Dir)
    end.

config(Key) ->
    config(Key, undefined).

config(Key, Default) ->
    application:get_env(ds_api, Key, Default).
