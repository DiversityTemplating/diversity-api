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
            Projects = case QueryVal of
                undefined ->
                    PublicProjects = divapi_cache:get_components(),
                    get_components_information(PublicProjects);
                Group when is_binary(Group) ->
                    PublicProjects = divapi_cache:get_components(),
                    filter_projects_by_grouping(PublicProjects, Group)
            end,
            cowboy_req:reply(200,
                [{<<"content-type">>, <<"application/json">>}],
                jiffy:encode(Projects),
                Req3);
        _ ->
            cowboy_req:reply(404, Req2)
    end,
	{ok, Req4, State}.

%% @doc Returns a list with new maps only containing the keys in information_fields
-spec get_components_information([binary()]) -> [map()].
get_components_information(Components) ->
    lists:foldl(
        fun({_,undefined}, Acc) -> Acc;
           ({_,ComponentJson}, Acc) ->
        DiversityMap =  jiffy:decode(ComponentJson, [return_maps]),
        Without = lists:filter(fun(K) -> not lists:member(K, ?INFORMATION_FIELDS) end, maps:keys(DiversityMap)),
        [maps:without(Without, DiversityMap) | Acc]
        end, [], Components).

%% @doc Returns a list of component names filtered by given grouping
-spec filter_projects_by_grouping([binary()], binary()) -> [binary()].
filter_projects_by_grouping(Components, Grouping) ->
    lists:foldl(fun({ComponentName, ComponentJson},  Acc) ->
        DiversityMap = jiffy:decode(ComponentJson, [return_maps]),
        Groups = maps:get(<<"grouping">>, DiversityMap, []),
        case lists:member(Grouping, Groups) of
            true -> [ComponentName | Acc];
            false -> Acc
        end
    end, [], Components).

terminate(_Reason, _Req, _State) ->
	ok.
