-module(ds_api).

-export([accepting/0]).
-export([components_dir/0]).
-export([config/1]).
-export([config/2]).
-export([set_config/2]).

accepting() ->
    config(accepting, false).

components_dir() ->
    case config(components_dir) of
        Dir when is_binary(Dir) -> Dir;
        Dir when is_list(Dir)   -> unicode:characters_to_binary(Dir);
        undefined               -> "./components"
    end.

config(Key) ->
    config(Key, undefined).

config(Key, Default) ->
    application:get_env(ds_api, Key, Default).

set_config(Key, Value) ->
    application:set_env(ds_api, Key, Value).
