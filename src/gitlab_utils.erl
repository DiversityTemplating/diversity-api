-module(gitlab_utils).


%% API
-export([get_public_projects/0, get_public_project_url/1]).

-define(GITLAB_URL, "http://git.diversity.io/api/v3/").
-define(GIT_URL_KEY, <<"http_url_to_repo">>).

%% @doc Searches for all public projects using the diversity gitlab api
-spec get_public_projects() -> map().
get_public_projects() ->
    {ok, Token} = application:get_env(divapi, token),
    Request = ?GITLAB_URL ++ "projects/search/%25" ++ "?private_token=" ++ Token,
    case httpc:request(Request) of
        {ok, {_, _, Res}} ->
            Projects = jiffy:decode(Res, [return_maps]),
            lists:foldl(fun(Map, AccMap) ->
                Name = maps:get(<<"name">>, Map),
                Url = maps:get(?GIT_URL_KEY, Map),
                maps:put(Name, Url, AccMap)
            end, maps:new(), Projects)
    end.

%% @doc Returns the url found by using the diversity gitlab api for a given project name
-spec get_public_project_url(binary()) -> binary().
get_public_project_url(ProjectName) ->
    Projects = get_public_projects(),
    maps:get(ProjectName, Projects).