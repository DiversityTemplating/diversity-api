-module(ds_api_component).

-export([components/0]).
-export([versions/1]).

-export([component_dir/1]).
-export([git_dir/1]).
-export([versions_dir/1]).
-export([version_dir/2]).

-export([files_dir/1]).
-export([remote_dir/1]).
-export([sass_dir/1]).

-export([make_version_dir/1]).

-export([add_component/2]).
-export([delete_component/1]).

-export([add_version/2]).
-export([delete_version/2]).

components() ->
    {ok, Components} = file:list_dir(ds_api:components_dir()),
    ordsets:from_list([unicode:characters_to_binary(Component) || Component <- Components]).

versions(Component) ->
    case file:list_dir(versions_dir(Component)) of
        {ok, Versions0} ->
            Versions1 = [begin
                             Version1 = unicode:characters_to_binary(Version0),
                             {ok, Version2} = ds_api_version:to_version(Version1),
                             Version2
                          end || Version0 <- Versions0],
            ordsets:from_list(Versions1);
        _Error ->
            undefined
    end.

component_dir(Component) ->
    ComponentsDir = ds_api:components_dir(),
    filename:join(ComponentsDir, Component).

git_dir(Component) ->
    filename:join(component_dir(Component), git).

versions_dir(Component) ->
    filename:join(component_dir(Component), versions).

version_dir(Component, Version) ->
    filename:join(versions_dir(Component), ds_api_version:to_binary(Version)).

files_dir(VersionDir) ->
    filename:join(VersionDir, files).

remote_dir(VersionDir) ->
    filename:join(VersionDir, remote).

sass_dir(VersionDir) ->
    filename:join(VersionDir, sass).

add_component(Component, RepoURL) ->
    ComponentDir = ds_api_component:component_dir(Component),

    TmpDir = ds_api_util:tmp_path(),
    ok = file:make_dir(TmpDir),

    VersionsDir = filename:join(TmpDir, versions),
    ok = file:make_dir(VersionsDir),

    GitDir = filename:join(TmpDir, git),
    case ds_api_git:clone(RepoURL, GitDir) of
        ok ->
            file:rename(TmpDir, ComponentDir);
        Error ->
            ds_api_util:delete_dir(TmpDir),
            Error
    end.

delete_component(Component) ->
    ComponentDir = ds_api_component:component_dir(Component),
    TmpDir = ds_api_util:tmp_path(),
    ok = file:rename(ComponentDir, TmpDir),
    ds_api_util:delete_dir(TmpDir),
    ok.

add_version(Component, Version) ->
    VersionDir = ds_api_component:version_dir(Component, Version),
    TmpDir = ds_api_util:tmp_path(),
    ok = file:make_dir(TmpDir),
    case create_version(Component, Version, TmpDir) of
        ok ->
            file:rename(TmpDir, VersionDir);
        Error ->
            ds_api_util:delete_dir(TmpDir),
            Error
    end.

create_version(Component, Version, TmpDir) ->
    GitDir = git_dir(Component),
    case get_git_versions(GitDir) of
        {ok, Versions} ->
            case ordsets:is_element(Version, Versions) of
                true ->
                    ok = make_version_dir(TmpDir),
                    VersionBin = ds_api_version:to_binary(Version),
                    FilesDir = files_dir(TmpDir),
                    case ds_api_git:copy_tag(GitDir, VersionBin, FilesDir) of
                        ok    -> ds_api_preprocess:run(Component, Version, TmpDir);
                        Error -> Error
                    end;
                false ->
                    {error, <<"Version does not exist as a tag.">>}
            end;
        Error ->
            Error
    end.

make_version_dir(Directory) ->
    ok = file:make_dir(files_dir(Directory)),
    ok = file:make_dir(remote_dir(Directory)),
    ok = file:make_dir(sass_dir(Directory)).

delete_version(Component, Version) ->
    VersionDir = ds_api_component:version_dir(Component, Version),
    TmpDir = ds_api_util:tmp_path(),
    ok = file:rename(VersionDir, TmpDir),
    ds_api_util:delete_dir(TmpDir),
    ok.

get_git_versions(GitDir) ->
    case ds_api_git:tags(GitDir) of
        {ok, Tags} ->
            GitVersions = lists:filtermap(
                            fun (Tag) ->
                                    case ds_api_version:to_version(Tag) of
                                        {ok, V}         -> {true, V};
                                        {error, badarg} -> false
                                    end
                            end,
                            Tags
                           ),
            {ok, ordsets:from_list(GitVersions)};
        Error ->
            Error
    end.
