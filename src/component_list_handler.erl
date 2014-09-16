-module(component_list_handler).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {
}).

init(_, Req, _Opts) ->
	{ok, Req, #state{}}.

handle(Req, State=#state{}) ->
    {Path, Req2} = cowboy_req:path(Req),
    {ok, Req4} = case Path of
        <<"/components/">> ->
            {QueryVal, Req3} = cowboy_req:qs_val(<<"grouping">>, Req2),
            Projects = case QueryVal of
                undefined ->
                    maps:keys(gitlab_utils:get_public_projects());
                Group when is_binary(Group) ->
                    PublicProjects = gitlab_utils:get_public_projects(),
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

filter_projects_by_grouping(Projects, Grouping) ->
    maps:fold(fun(ProjectName, ProjectUrl, Acc) ->
        Json = git_utils:get_diversity_json(ProjectName, ProjectUrl, <<"HEAD">>),
        DiversityMap = jiffy:decode(Json, [return_maps]),
        Groups = maps:get(<<"grouping">>, DiversityMap, []),
        case lists:member(Grouping, Groups) of
            true -> [ProjectName | Acc];
            false -> Acc
        end
    end, [], Projects).

terminate(_Reason, _Req, _State) ->
	ok.
