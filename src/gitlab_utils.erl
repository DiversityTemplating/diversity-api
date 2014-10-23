-module(gitlab_utils).


%% API
-export([get_public_projects/0, get_public_project_url/1]).

-define(GITLAB_URL, "http://git.diversity.io/api/v3/").
-define(GIT_URL_KEY, <<"http_url_to_repo">>).

%% @doc Searches for all public projects using the diversity gitlab api
-spec get_public_projects() -> map().
get_public_projects() ->
    {ok, Token} = application:get_env(divapi, token),
    Projects = get_public_projects(Token, 1, []),
    lists:foldl(fun (Map, AccMap) ->
        Name = maps:get(<<"name">>, Map),
        Url = maps:get(?GIT_URL_KEY, Map),
        maps:put(Name, Url, AccMap)
    end, maps:new(), Projects).

get_public_projects(Token, Page, Acc) ->
    Request = ?GITLAB_URL ++ "projects/search/%25" ++ "?private_token=" ++ Token ++ "&page=" ++ integer_to_list(Page),
    case httpc:request(Request) of
        {ok, {_, _, Res}} ->
            case jiffy:decode(Res, [return_maps]) of
                [] -> lists:flatten(Acc);
                Projects -> get_public_projects(Token, Page + 1, [Projects | Acc])
            end;
        Error ->
            Error
    end.

%% @doc Returns the url found by using the diversity gitlab api for a given project name
-spec get_public_project_url(binary()) -> binary().
get_public_project_url(ProjectName) ->
    Projects = get_public_projects(),
    maps:get(ProjectName, Projects).
