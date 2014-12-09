-module(component_list_handler).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {
}).

-define(INFORMATION_FIELDS, [<<"name">>,<<"title">>,<<"description">>,<<"version">>,<<"grouping">>]).

init(_, Req, _Opts) ->
    {ok, Req, #state{}}.

handle(Req, State=#state{}) ->
    {Path, Req2} = cowboy_req:path(Req),
    {ok, Req4} = case Path of
        <<"/components/">> ->
            {QueryVal, Req3} = cowboy_req:qs_val(<<"grouping">>, Req2),
            {ok, RepoDir} = application:get_env(divapi, repo_dir),
            {ok, PublicProjects} = file:list_dir(RepoDir),
            Projects = case QueryVal of
                undefined ->
                    get_components_information(PublicProjects);
                Group when is_binary(Group) ->
                    filter_projects_by_grouping(PublicProjects, Group)
            end,
            cowboy_req:reply(200,
                [{<<"content-type">>, <<"application/json">>},
				 {<<"Access-Control-Allow-Origin">>, <<"*">>}],
                jiffy:encode(Projects),
                Req3);
        _ ->
            cowboy_req:reply(404, Req2)
    end,
    {ok, Req4, State}.

%% @doc Returns a list with new maps only containing the keys in information_fields
-spec get_components_information([binary()]) -> [map()].
get_components_information(Components) ->
    {ok, RepoDir} = application:get_env(divapi, repo_dir),
    lists:foldl(
        fun(Component, Acc) ->
            case filelib:is_dir(RepoDir ++ "/" ++ Component) of
                true ->
                    ComponentAdjusted =
                        unicode:characters_to_binary(string:left(Component, length(Component) - 4)),
                    case git_utils:get_diversity_json(ComponentAdjusted, <<"HEAD">>) of
                        undefined ->
                            Acc;
                        ComponentJson ->
                            DiversityMap =  jiffy:decode(ComponentJson, [return_maps]),
                            Without = lists:filter(
                                fun(K) ->
                                    not lists:member(K, ?INFORMATION_FIELDS)
                                end, maps:keys(DiversityMap)
                            ),
                            [maps:without(Without, DiversityMap) | Acc]
                    end;
                _ -> Acc
            end
        end, [], Components).

%% @doc Returns a list of component names filtered by given grouping
-spec filter_projects_by_grouping([binary()], binary()) -> [binary()].
filter_projects_by_grouping(Components, Grouping) ->
    {ok, RepoDir} = application:get_env(divapi, repo_dir),
    lists:foldl(fun(Component,  Acc) ->
        case filelib:is_dir(RepoDir ++ "/" ++ Component) of
            true ->
                ComponentAdjusted =
                    unicode:characters_to_binary(string:left(Component, length(Component) - 4)),
                case git_utils:get_diversity_json(ComponentAdjusted, <<"HEAD">>) of
                    undefined ->
                        Acc;
                    ComponentJson ->
                        DiversityMap = jiffy:decode(ComponentJson, [return_maps]),
                        Groups = maps:get(<<"grouping">>, DiversityMap, []),
                        case lists:member(Grouping, Groups) of
                            true -> [ComponentAdjusted | Acc];
                            false -> Acc
                        end
                end;
            _ ->
                Acc
        end
    end, [], Components).

terminate(_Reason, _Req, _State) ->
    ok.
