-module(ds_api_component_handler).

-export([init/3]).
-export([rest_init/2]).
-export([allowed_methods/2]).
-export([resource_exists/2]).
-export([is_authorized/2]).
-export([is_conflict/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([delete_resource/2]).
-export([to_json/2]).
-export([handle_add_component/2]).

init(_Type, Req, []) ->
    {upgrade, protocol, cowboy_rest, Req, no_state}.

rest_init(Req0, no_state) ->
    {Component, Req1} = cowboy_req:binding(component, Req0),
    {ok, Req1, Component}.

allowed_methods(Req, Component) ->
    {[<<"GET">>, <<"HEAD">>, <<"PUT">>, <<"DELETE">>, <<"OPTIONS">>], Req, Component}.

resource_exists(Req, Component) ->
    {filelib:is_dir(ds_api_component:component_dir(Component)), Req, Component}.

is_authorized(Req0, Component) ->
    case cowboy_req:method(Req0) of
        {Method, Req1} when Method =:= <<"PUT">>;
                            Method =:= <<"DELETE">> ->
            % TODO: API-KEY
            {true, Req1, Component};
        _ ->
            {true, Req0, Component}
    end.

is_conflict(Req, Component) ->
    {filelib:is_dir(ds_api_component:component_dir(Component)), Req, Component}.

content_types_provided(Req, Component) ->
    {[{{<<"application">>, <<"json">>, []}, to_json}], Req, Component}.

content_types_accepted(Req, Component) ->
    {[{{<<"application">>, <<"x-www-form-urlencoded">>, []}, handle_add_component}], Req, Component}.

delete_resource(Req, Component) ->
    ok = ds_api_component:delete_component(Component),
    {true, Req, Component}.

to_json(Req, Component) ->
    Versions = [ds_api_version:to_binary(Version) || Version <- ds_api_component:versions(Component)],
    {jiffy:encode(Versions), Req, Component}.

handle_add_component(Req0, Component) ->
    {ok, Query, Req1} = cowboy_req:body_qs(Req0),
    case proplists:get_value(<<"repo_url">>, Query) of
        RepoURL when is_binary(RepoURL) ->
            case ds_api_component:add_component(Component, RepoURL) of
                ok ->
                    {true, Req1, Component};
                {error, Error} ->
                    {false, cowboy_req:set_resp_body(Error, Req1), Component}
            end;
        undefined ->
            Req2 = cowboy_req:set_resp_body(<<"No repository URL specified.">>, Req1),
            {false, Req2, Component}
    end.
