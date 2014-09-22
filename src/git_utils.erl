-module(git_utils).

%% API
-export([tags/2, get_diversity_json/3, git_refresh_repo/1, get_file/4]).

%% @doc Returns a list with all tags in a git repository
-spec tags(binary(), binary()) -> [binary()].
tags(RepoName, RepoUrl) ->
    Cmd = "tag",
    ResultString = get_git_result(RepoName, RepoUrl, Cmd),
    [list_to_binary(Tag) || Tag <- string:tokens(ResultString, "\n")].

%% @doc Returns the diversity.json for a tag in a repo
-spec get_diversity_json(binary(), binary(), binary()) -> binary().
get_diversity_json(RepoName, RepoUrl, Tag) ->
    get_git_file(RepoName, RepoUrl, Tag, <<"diversity.json">>).

%% @doc Fetches the latest tags for a repo
-spec git_refresh_repo(binary()) -> any().
git_refresh_repo(RepoName) ->
    {ok, RepoDir} = application:get_env(divapi, repo_dir),
    GitRepoName =  RepoDir ++ "/" ++ binary_to_list(RepoName) ++ ".git",
    file:set_cwd(GitRepoName),
    Cmd = "fetch origin master:master",
    git_cmd(Cmd).

get_file(RepoName, RepoUrl, Tag, FilePath) ->
    get_git_file(RepoName, RepoUrl, Tag, FilePath).


%% ----------------------------------------------------------------------------
%% Internal stuff
%% ----------------------------------------------------------------------------
get_git_file(RepoName, RepoUrl, Tag, FilePath) ->
    Cmd = "show " ++ binary_to_list(Tag) ++ ":" ++ binary_to_list(FilePath),
    case get_git_result(RepoName, RepoUrl, Cmd) of
        "fatal" ++ _ -> undefined;
        Result -> list_to_binary(Result)
    end.

get_git_result(RepoName, RepoUrl, Cmd) ->
    {ok, RepoDir} = application:get_env(divapi, repo_dir),
    GitRepoName =  RepoDir ++ "/" ++ binary_to_list(RepoName) ++ ".git",
    %% Clone git repo if non-existing in configured dir
    case filelib:is_dir(GitRepoName) of
        false ->
            RepoUrl2 = get_repo_url(RepoName, RepoUrl),
            clone_bare(binary_to_list(RepoUrl2));
        true ->
            %% Checkout not needed
            ok
    end,
    file:set_cwd(GitRepoName),
    git_cmd(Cmd).

clone_bare(RepoUrl) ->
    {ok, RepoDir} = application:get_env(divapi, repo_dir),
    filelib:ensure_dir(RepoDir),
    file:set_cwd(RepoDir),
    Cmd = "clone --bare " ++ RepoUrl,
    git_cmd(Cmd).

get_repo_url(RepoName, undefined) -> gitlab_utils:get_public_project_url(RepoName);
get_repo_url(_, RepoUrl) -> RepoUrl.

git_cmd(Cmd) ->
    os:cmd("git " ++ Cmd).