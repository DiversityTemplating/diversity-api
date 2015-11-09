-module(ds_api_component_handler).

-export([init/2]).
-export([allowed_methods/2]).
-export([resource_exists/2]).
-export([is_authorized/2]).
-export([is_conflict/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([delete_resource/2]).
-export([to_json/2]).
-export([handle_add_component/2]).

init(Req0, []) ->
    Req1 = ds_api_util:set_access_control_headers(Req0),
    {cowboy_rest, Req1, cowboy_req:binding(component, Req1)}.

allowed_methods(Req, Component) ->
    Methods = [<<"GET">>, <<"HEAD">>, <<"PUT">>, <<"DELETE">>, <<"OPTIONS">>],
    {Methods, Req, Component}.

resource_exists(Req, Component) ->
    {component_exists(Component), Req, Component}.

is_authorized(Req, Component) ->
    case cowboy_req:method(Req) of
        Method when Method =:= <<"PUT">>;
                    Method =:= <<"DELETE">> ->
            {ds_api_auth:is_authorized(Req), Req, Component};
        _ ->
            {true, Req, Component}
    end.

is_conflict(Req, Component) ->
    {component_exists(Component), Req, Component}.

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
                {error, _Error} ->
                    {false, Req1, Component}
            end;
        undefined ->
            {false, Req1, Component}
    end.

component_exists(Component) ->
    filelib:is_dir(ds_api_component:component_dir(Component)).
