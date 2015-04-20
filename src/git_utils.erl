-module(git_utils).

%% API
-export([tags/1, get_diversity_json/2, refresh_repo/1, get_file/3, clone_bare/1]).


%%% TODO: All git utils will be needing a working dir sent to each path.
%%%       This module should not need to know anything about the application!


%% @doc Returns a list with all tags in a git repository
-spec tags(binary()) -> [binary()].
tags(RepoName) ->
    Cmd = <<"git tag">>,
    ComponentDir = get_component_dir(RepoName),
    case git_cmd(Cmd, ComponentDir) of
        error ->
            [];
        <<>>  ->
            [];
        ResultBin ->
            Res = re:split(ResultBin, <<"\n">>),
            lists:droplast(Res)
    end.

%% @doc Returns the diversity.json for a tag in a repo
-spec get_diversity_json(binary(), binary()) -> binary().
get_diversity_json(RepoName, Tag) ->
    get_git_file(RepoName, Tag, <<"diversity.json">>).

%% @doc Fetches the latest tags for a repo
-spec refresh_repo(binary()) -> any().
refresh_repo(RepoName) ->
    {ok, RepoDir} = application:get_env(divapi, repo_dir),
    GitRepoName =  RepoDir ++ "/" ++ binary_to_list(RepoName) ++ ".git",
    Cmd = <<"git fetch origin master:master">>,
    git_cmd(Cmd, GitRepoName).

get_file(RepoName, Tag, FilePath) ->

    get_git_file(RepoName, Tag, FilePath).

%% ----------------------------------------------------------------------------
%% Internal stuff
%% ----------------------------------------------------------------------------

%% @doc Retrieves the file from the bare git repo and the specific tag.
-spec get_git_file(RepoName :: binary(), Tag :: binary(),
                   FilePath :: binary) -> binary() | undefined.
get_git_file(RepoName, Tag, FilePath) ->
    case divapi:is_prod() of
        true ->
            %% Read the file!!
            ComponentDir = get_component_dir(RepoName),
            Path = filename:join(ComponentDir, FilePath),
            file:read(Path);
        false ->
            Cmd = <<"git --no-pager show ", Tag/binary, ":", FilePath/binary>>,
            ComponentDir = get_component_dir(RepoName),
            case git_cmd(Cmd, ComponentDir) of
                FileBin when is_binary(FileBin) -> FileBin;
                error                           -> undefined;
                ok                              -> <<>> %% Command succesful but empty result!
            end
    end.

get_component_dir(RepoName) ->
    {ok, RepoDir} = application:get_env(divapi, repo_dir),
    RepoDir1 = case is_binary(RepoDir) of
        true -> RepoDir;
        false -> unicode:character_to_binary(RepoDir)
    end,
    filename:join(RepoDir1, <<"/", RepoName/binary, ".git">>).

clone_bare(RepoUrl) ->
    {ok, RepoDir} = application:get_env(divapi, repo_dir),
    %% Ensure it exists if not try to create it.
    ok = filelib:ensure_dir(RepoDir ++ "/"),
    Cmd = <<"git clone --bare ", RepoUrl/binary>>,
    git_cmd(Cmd, RepoDir).

git_cmd(Cmd, WorkingDir) ->
    Port = erlang:open_port({spawn, Cmd}, [exit_status, {cd, WorkingDir}, binary, stderr_to_stdout]),
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
