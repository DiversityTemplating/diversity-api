-module(ds_api).

-export([accept/1]).
-export([accepting/0]).

-export([root_dir/0]).
-export([components_dir/0]).
-export([tmp_dir/0]).

-export([config/1]).
-export([config/2]).
-export([set_config/2]).

accept(Accept) when is_boolean(Accept) ->
    set_config(accept, Accept).

accepting() ->
    config(accept, false).

root_dir() ->
    case config(root_dir) of
        Dir when is_binary(Dir) -> Dir;
        Dir when is_list(Dir)   -> unicode:characters_to_binary(Dir)
    end.

components_dir() ->
    filename:join(root_dir(), components).

tmp_dir() ->
    filename:join(root_dir(), tmp).

config(Key) ->
    config(Key, undefined).

config(Key, Default) ->
    application:get_env(ds_api, Key, Default).

set_config(Key, Value) ->
    application:set_env(ds_api, Key, Value).
