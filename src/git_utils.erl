-module(git_utils).

%% API
-export([clone_bare/1, tags/1, get_diversity_json/2, git_cmd/1]).

%% @doc Clones a git repository with the bare flag
-spec clone_bare(string()) -> any().
clone_bare(RepoUrl) ->
    {ok, RepoDir} = application:get_env(divapi, repo_dir),
    filelib:ensure_dir(RepoDir),
    file:set_cwd(RepoDir),
    Cmd = "clone --bare " ++ RepoUrl,
    git_cmd(Cmd).

%% @doc Returns a list with all tags in a git repository
-spec tags(binary()) -> [binary()].
tags(RepoName) ->
    Cmd = "tag",
    ResultString = get_git_result(RepoName, Cmd),
    [list_to_binary(Tag) || Tag <- string:tokens(ResultString, "\n")].

get_diversity_json(RepoName, Tag) ->
    Cmd = "show " ++ binary_to_list(Tag) ++ ":diversity.json",
    get_git_result(RepoName, Cmd).

get_git_result(RepoName, Cmd) ->
    {ok, RepoDir} = application:get_env(divapi, repo_dir),
    GitRepoName =  RepoDir ++ "/" ++ binary_to_list(RepoName) ++ ".git",
    %% Clone git repo if non-existing in configured dir
    case filelib:is_dir(GitRepoName) of
        false ->
            RepoUrl = gitlab_utils:get_public_project_url(RepoName),
            clone_bare(binary_to_list(RepoUrl));
        true ->
            %% Checkout not needed
            ok
    end,
    file:set_cwd(GitRepoName),
    git_cmd(Cmd).

git_cmd(Cmd) ->
    os:cmd("git " ++ Cmd).