-module(git_utils).

%% API
-export([tags/2, get_diversity_json/3, git_refresh_repo/1, get_file/4]).

%% @doc Returns a list with all tags in a git repository
-spec tags(binary(), binary()) -> [binary()].
tags(RepoName, RepoUrl) ->
    Cmd = <<"git tag">>,
    case get_git_result(RepoName, RepoUrl, Cmd) of
        error ->
            [];
        <<>>  ->
            [];
        ResultBin ->
            Res = re:split(ResultBin, <<"\n">>),
            lists:droplast(Res)
    end.

%% @doc Returns the diversity.json for a tag in a repo
-spec get_diversity_json(binary(), binary(), binary()) -> binary().
get_diversity_json(RepoName, RepoUrl, Tag) ->
    get_git_file(RepoName, RepoUrl, Tag, <<"diversity.json">>).

%% @doc Fetches the latest tags for a repo
-spec git_refresh_repo(binary()) -> any().
git_refresh_repo(RepoName) ->
    {ok, RepoDir} = application:get_env(divapi, repo_dir),
    GitRepoName =  RepoDir ++ "/" ++ binary_to_list(RepoName) ++ ".git",
    Cmd = <<"git fetch origin master:master">>,
    git_cmd(Cmd, GitRepoName).

get_file(RepoName, RepoUrl, Tag, FilePath) ->
    get_git_file(RepoName, RepoUrl, Tag, FilePath).

%% ----------------------------------------------------------------------------
%% Internal stuff
%% ----------------------------------------------------------------------------

%% @doc Retrieves the file from the bare git repo and the specific tag.
-spec get_git_file(RepoName :: binary(), RepoUrl :: binary(), Tag :: binary(),
                   FilePath :: binary) -> binary() | undefined.
get_git_file(RepoName, RepoUrl, Tag, FilePath) ->
    Cmd = <<"git --no-pager show ", Tag/binary, ":", FilePath/binary>>,
    case get_git_result(RepoName, RepoUrl, Cmd) of
        FileBin when is_binary(FileBin) -> FileBin;
        error                           -> undefined;
        ok                              -> <<>> %% Command succesful but empty result!
    end.

get_git_result(RepoName, RepoUrl, Cmd) ->
    {ok, RepoDir} = application:get_env(divapi, repo_dir),
    GitRepoDir =  RepoDir ++ "/" ++ binary_to_list(RepoName) ++ ".git",
    %% Clone git repo if non-existing in configured dir
    case filelib:is_dir(GitRepoDir) of
        false ->
            RepoUrl2 = get_repo_url(RepoName, RepoUrl),
            clone_bare(binary_to_list(RepoUrl2));
        true ->
            %% Checkout not needed
            ok
    end,
    git_cmd(Cmd, GitRepoDir).

clone_bare(RepoUrl) ->
    {ok, RepoDir} = application:get_env(divapi, repo_dir),
    %% Ensure it exists if not try to create it.
    ok = filelib:ensure_dir(RepoDir ++ "/"),
    Cmd = <<"git clone --bare ", (list_to_binary(RepoUrl))/binary>>,
    git_cmd(Cmd, RepoDir).

get_repo_url(RepoName, undefined) -> gitlab_utils:get_public_project_url(RepoName);
get_repo_url(_, RepoUrl) -> RepoUrl.

git_cmd(Cmd, WorkingDir) ->
    Port = erlang:open_port({spawn, Cmd}, [exit_status, {cd, WorkingDir}, binary, use_stdio]),
    wait_for_file(Port, <<>>).

wait_for_file(Port, File) ->
    receive
        {Port, {data, Chunk}} ->
            wait_for_file(Port, <<File/binary, Chunk/binary>>);
        {_ , {exit_status, 0}} ->
            File;
        {_, {exit_status, _}} ->
            %% Either not a git repo or operation failed. No need to close port it' already done.
            error
    end.
