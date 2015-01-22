-module(component_action).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {action}).

-define(ACCESS_CONTROL_HEADER, [{<<"Access-Control-Allow-Origin">>, <<"*">>}]).

init(_, Req, [Opts]) ->
    {ok, Req, #state{action = Opts}}.

handle(Req, State = #state{action = Action}) ->
    {ComponentName, Req1} = cowboy_req:binding(component, Req),
    case Action of
        register -> register_component(Req1);
        update   -> update_component(Req1, ComponentName)
    end,
    {ok, Req1, State}.

register_component(Req) ->
    case fetch_get_post_data(Req) of
        {ok, RegisterData} ->
            case proplists:get_value(<<"repo_url">>, RegisterData) of
                undefined ->
                    send_reply(Req, error, <<"No repository url supplied">>);
                RepoUrl ->
                    case git_utils:clone_bare(RepoUrl) of
                        error ->
                            send_reply(Req, error,
                                       <<"Failed to register your diversity"
                                         " component.">>);
                        _ ->
                            %% Successful registration, send to all other nodes.
                            rpc:eval_everywhere(git_utils, clone_bare, [RepoUrl]),
                            send_reply(Req, success,
                                       <<"Successfully registered your"
                                         " diversity component.">>)
                    end
            end;
        {error, Msg}   -> send_reply(Req, error, Msg)
    end.

update_component(Req, ComponentName) ->
    case component_exists(ComponentName) of
        true ->
            case git_utils:refresh_repo(ComponentName) of
                error ->
                    send_reply(Req, error, <<"Failed to update ",
                               ComponentName/binary>>);
                _ ->
                    %% Successful update, send to all other nodes as well.
                    rpc:eval_everywhere(git_utils, refresh_repo, [ComponentName]),
                    send_reply(Req, success, <<"Component has been updated">>)
            end;
        false ->
            send_reply(Req, error, <<"Component does not exists.">>)
    end.

component_exists(ComponentName) ->
    {ok, RepoDir} = application:get_env(divapi, repo_dir),
    filelib:is_dir(RepoDir ++ "/" ++ binary_to_list(ComponentName) ++ ".git").

send_reply(Req, error, Msg) ->
    cowboy_req:reply(500, ?ACCESS_CONTROL_HEADER, Msg, Req);
send_reply(Req, success, Msg) ->
    cowboy_req:reply(200, ?ACCESS_CONTROL_HEADER, Msg, Req).

-spec fetch_get_post_data(cowboy_req:req()) -> {ok, [{binary(), term()}]} | {error, binary()}.
fetch_get_post_data(Req) ->
    %% Check that we have a body otherwise check query string values
    case cowboy_req:has_body(Req) of
        true ->
            case cowboy_req:body_qs(Req) of
                {ok, [], _Req}       -> {error, "Missing params"};
                {ok, PostVals, _Req} -> {ok, PostVals}
            end;
        false ->
            {error, "Missing params"}
    end.

terminate(_Reason, _Req, _State) ->
    ok.
