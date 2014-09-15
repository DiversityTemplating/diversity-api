-module(git_utils).

%% API
-export([clone_bare/1, tags/1]).

%% @doc Clones a git repository with the bare flag
clone_bare(RepoUrl) ->
    {ok, RepoDir} = application:get_env(divapi, repo_dir),
    filelib:ensure_dir(RepoDir),
    c:cd(RepoDir),
    os:cmd("git clone --bare " ++ RepoUrl).

%% @doc Returns a list with all tags in a git repository
-spec tags(binary()) -> [binary()].
tags(RepoName) ->
    {ok, RepoDir} = application:get_env(divapi, repo_dir),
    GitRepoName = binary_to_list(RepoName) ++ ".git",

    case filelib:is_dir(GitRepoName) of
        false ->
            RepoUrl = gitlab_utils:get_public_project_url(RepoName),
            clone_bare(binary_to_list(RepoUrl))
    end,

    c:cd(RepoDir ++ "/" ++ GitRepoName),
    Cmd = "git tag",
    ResultString = os:cmd(Cmd),
    [list_to_binary(Tag) || Tag <- string:tokens(ResultString, "\n")].