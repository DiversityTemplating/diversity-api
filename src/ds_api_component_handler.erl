-module(ds_api_component_handler).

-export([init/3]).
-export([allowed_methods/2]).
-export([resource_exists/2]).
-export([is_authorized/2]).
-export([is_conflict/2]).
-export([generate_etag/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([delete_resource/2]).
-export([to_json/2]).
-export([add_component/2]).

init(_Type, Req0, []) ->
    {Component, Req1} = cowboy_req:binding(component, Req0),
    {upgrade, protocol, cowboy_rest, Req1, Component}.

allowed_methods(Req, Component) ->
    Methods = [<<"GET">>, <<"HEAD">>, <<"PUT">>, <<"DELETE">>, <<"OPTIONS">>],
    {Methods, Req, Component}.

resource_exists(Req, Component) ->
    {ds_api_component_mgr:exists(Component), Req, Component}.

generate_etag(Req, Component) ->
    {{strong, <<"components/", Component/binary, "/versions">>}, Req, Component}.

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
    {ds_api_component_mgr:exists(Component), Req, Component}.

content_types_provided(Req, Component) ->
    {[{{<<"application">>, <<"json">>, []}, to_json}], Req, Component}.

content_types_accepted(Req, Component) ->
    {[{{<<"application">>, <<"x-www-form-urlencoded">>, []}, add_component}], Req, Component}.

delete_resource(Req, Component) ->
    case ds_api_component_mgr:delete_component(Component) of
        ok              -> {true, Req, Component};
        {error, _Error} -> {false, Req, Component}
    end.

to_json(Req, Component) ->
    Versions0 = ds_api_component_mgr:versions(Component),
    Versions1 = [ds_api_util:to_binary(Version) || Version <- Versions0],
    {jiffy:encode(Versions1), Req, Component}.

add_component(Req0, Component) ->
    {ok, QS, Req1} = cowboy_req:body_qs(Req0),
    case proplists:get_value(<<"repo_url">>, QS) of
        RepoURL when is_binary(RepoURL) ->
            case ds_api_component_mgr:add_component(Component, RepoURL) of
                ok ->
                    {true, Req1, Component};
                {error, _Error} ->
                    {false, Req1, Component}
            end;
        undefined ->
            {false, Req1, Component}
    end.
