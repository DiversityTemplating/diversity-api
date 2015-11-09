-module(ds_api).

-export([accept/1]).
-export([accepting/0]).

-export([api_keys/0]).

-export([root_dir/0]).
-export([root_dir/1]).
-export([components_dir/0]).
-export([tmp_dir/0]).

-export([fallback_api/0]).

-export([config/1]).
-export([config/2]).
-export([set_config/2]).

accept(Accept) when is_boolean(Accept) ->
    set_config(accept, Accept).

accepting() ->
    config(accept, false).

api_keys() ->
    config(api_keys).

root_dir() ->
    case config(root_dir) of
        Dir when is_binary(Dir) -> Dir;
        Dir when is_list(Dir)   -> unicode:characters_to_binary(Dir)
    end.

root_dir(NewRootDir) ->
    set_config(root_dir, NewRootDir).

components_dir() ->
    filename:join(root_dir(), components).

tmp_dir() ->
    filename:join(root_dir(), tmp).

fallback_api() ->
    config(fallback_api).

config(Key) ->
    config(Key, undefined).

config(Key, Default) ->
    application:get_env(ds_api, Key, Default).

set_config(Key, Value) ->
    application:set_env(ds_api, Key, Value).
