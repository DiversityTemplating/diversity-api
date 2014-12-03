-module(component_event).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {}).

-define(ACCESS_CONTROL_HEADER, [{<<"Access-Control-Allow-Origin">>, <<"*">>}]).

init(_, Req, _Opts) ->
    {ok, Req, #state{}}.

handle(Req, State = #state{}) ->
    case cowboy_req:path(Req) of
        {<<"/register/">>, Req1} ->
            register_component(Req1);
        {<<"/update/">>, Req1} ->
            update_component(Req1)
    end,
    {ok, Req, State}.

register_component(Req) ->
    RegisterData = fetch_get_post_data(Req),
    case proplists:get_value(<<"repo_url">>, RegisterData) of
        undefined ->
            send_reply(Req, error, <<"No repository url supplied">>);
        RepoUrl ->
            case git_utils:clone_bare(RepoUrl) of
                error ->
                    send_reply(Req, error, <<"Failed to register your diversity component.">>);
                _ ->
                    send_reply(Req, success,
                               <<"Successfully registered your diversity component.">>)
            end
    end.

update_component(Req) ->
    UpdateData = fetch_get_post_data(Req),
    case proplists:get_value(<<"component">>, UpdateData) of
        undefined ->
            send_reply(Req, error, <<"No component name supplied.">>);
        ComponentName ->
            case git_utils:refresh_repo(ComponentName) of
                error ->
                    send_reply(Req, error, <<"Failed to update ", ComponentName/binary>>);
                _ ->
                    send_reply(Req, success, <<"Component has been updated">>)
            end
    end.

send_reply(Req, error, Msg) ->
    cowboy_req:reply(500, ?ACCESS_CONTROL_HEADER, Msg, Req);
send_reply(Req, success, Msg) ->
    cowboy_req:reply(200, ?ACCESS_CONTROL_HEADER, Msg, Req).

-spec fetch_get_post_data(cowboy_req:req()) -> [{binary(), term()}].
fetch_get_post_data(Req) ->
    %% Check that we have a body otherwise check query string values
    case cowboy_req:has_body(Req) of
        true ->
            {ok, PostVals, _Req} = cowboy_req:body_qs(Req),
            PostVals;
        false ->
            {QueryStringVals, _Req} = cowboy_req:qs_vals(Req),
            QueryStringVals
    end.

terminate(_Reason, _Req, _State) ->
    ok.
