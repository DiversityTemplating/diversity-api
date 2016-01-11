-module(ds_api_git).

-export([clone/2]).
-export([versions/1]).
-export([copy_version/3]).

%% @doc Clone a given git url into the specified directory
-spec clone(binary(), binary()) -> ok | {error, binary()}.
clone(RepoURL, GitDir) ->
    Command = os:find_executable(git),
    Opts = [{cd, filename:dirname(GitDir)}, {args, [<<"clone">>, RepoURL, filename:basename(GitDir)]}],
    case ds_api_util:cmd(Command, Opts) of
        {ok, _Reply} -> ok;
        Error        -> Error
    end.

%% @doc Retrive the available versions from the given repository
-spec versions(binary()) -> ordsets:ordset(ds_api_version:version()).
versions(GitDir) ->
    case update_versions(GitDir) of
        ok ->
            Command = os:find_executable(git),
            Opts = [{cd, GitDir}, {args, [<<"tag">>]}],
            case ds_api_util:cmd(Command, Opts) of
                {ok, Tags0} ->
                    Tags1 = binary:split(Tags0, <<$\n>>, [global, trim]),
                    Versions = lists:filtermap(
                                 fun (Tag) ->
                                         case ds_api_version:to_version(Tag) of
                                             {ok, Version}   -> {true, Version};
                                             {error, badarg} -> false
                                         end
                                 end,
                                 Tags1
                                ),
                    {ok, ordsets:from_list(Versions)};
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

%% @doc Copy a version from the git repository into the given output directory
-spec copy_version(binary(), ds_api_version:version(), binary()) -> ok | {error, term()}.
copy_version(GitDir, Version, OutputDir) ->
    Tag = ds_api_version:to_binary(Version),
    TempFile0 = filename:join(OutputDir, <<"version.zip">>),
    Command = os:find_executable(git),
    Args = [<<"archive">>, <<"-0">>, <<"-o">>, TempFile0, Tag],
    Opts = [{cd, GitDir}, {args, Args}],
    case ds_api_util:cmd(Command, Opts) of
        {ok, _Reply} ->
            %% zip does not like binary file names...
            TempFile1 = unicode:characters_to_list(TempFile0),
            case zip:extract(TempFile1, [{cwd, OutputDir}]) of
                {ok, _Files} -> file:delete(TempFile0);
                Error        -> Error
            end;
        Error ->
            Error
    end.

%% @doc Update all versions (tags) of the given git repository
-spec update_versions(binary()) -> ok | {error, binary()}.
update_versions(GitDir) ->
    Command = os:find_executable(git),
    Opts = [{cd, GitDir}, {args, [<<"fetch">>, <<"--prune">>, <<"origin">>, <<"+refs/tags/*:refs/tags/*">>]}],
    case ds_api_util:cmd(Command, Opts) of
        {ok, _Reply} -> ok;
        Error        -> Error
    end.
