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

-type component() :: binary().
-type version() :: ds_api_version:version().
-type path() :: binary().
-type url() :: binary().

%% @doc List all components
-spec components() -> ordsets:ordset(component()).
components() ->
    {ok, Components} = file:list_dir(ds_api:components_dir()),
    ordsets:from_list([unicode:characters_to_binary(Component) || Component <- Components]).

%% @doc List all versions of a component or undefined if the component does not exist
-spec versions(component()) -> ordsets:ordset(version()) | undefined.
versions(Component) ->
    ComponentDir = component_dir(Component),
    VersionsDir = versions_dir(ComponentDir),
    case file:list_dir(VersionsDir) of
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

%% @doc Get the path to a components directory
-spec component_dir(component()) -> path().
component_dir(Component) ->
    ComponentsDir = ds_api:components_dir(),
    filename:join(ComponentsDir, Component).

%% @doc Get the path to a components git repository
-spec git_dir(path()) -> path().
git_dir(ComponentDir) ->
    filename:join(ComponentDir, git).

%% @doc Get the path to a components versions directory
-spec versions_dir(path()) -> path().
versions_dir(ComponentDir) ->
    filename:join(ComponentDir, versions).

%% @doc Get the path to specific version directory
-spec version_dir(path(), version()) -> path().
version_dir(VersionsDir, Version) ->
    filename:join(VersionsDir, ds_api_version:to_binary(Version)).

%% @doc Get the path to the files directory given a version directory
-spec files_dir(path()) -> path().
files_dir(VersionDir) ->
    filename:join(VersionDir, files).

%% @doc Get the path to a versions remote directory
-spec remote_dir(path()) -> path().
remote_dir(VersionDir) ->
    filename:join(VersionDir, remote).

%% @doc Get the path to a versions sass directory
-spec sass_dir(path()) -> path().
sass_dir(VersionDir) ->
    filename:join(VersionDir, sass).

%% @doc Add a component with the given repository URL
-spec add_component(component(), url()) -> ok | {error, term()}.
add_component(Component, RepoURL) ->
    ComponentDir = component_dir(Component),

    %% Create a temporary directory to use to prepare the component before we release it
    TmpDir = ds_api_util:tmp_path(),
    ok = file:make_dir(TmpDir),

    VersionsDir = versions_dir(TmpDir),
    ok = file:make_dir(VersionsDir),

    GitDir = git_dir(TmpDir),
    case ds_api_git:clone(RepoURL, GitDir) of
        ok ->
            file:rename(TmpDir, ComponentDir);
        Error ->
            ds_api_util:delete_dir(TmpDir),
            Error
    end.

%% @doc Delete a component
-spec delete_component(component()) -> ok.
delete_component(Component) ->
    ComponentDir = component_dir(Component),
    TmpDir = ds_api_util:tmp_path(),
    ok = file:rename(ComponentDir, TmpDir),
    ds_api_util:delete_dir(TmpDir),
    ok.

%% @doc Add a version for a given component
-spec add_version(component(), version()) -> ok | {error, term()}.
add_version(Component, Version) ->
    ComponentDir = component_dir(Component),
    VersionsDir = versions_dir(ComponentDir),
    VersionDir = version_dir(VersionsDir, Version),
    TmpDir = ds_api_util:tmp_path(),
    ok = file:make_dir(TmpDir),

    %% Create the version but delete it if it fails
    case create_version(Component, Version, TmpDir) of
        ok ->
            file:rename(TmpDir, VersionDir);
        Error ->
            ds_api_util:delete_dir(TmpDir),
            Error
    end.

%% @doc Create version by copying a specific tag then preprocessing the directory. 
-spec create_version(component(), version(), path()) -> ok | {error, binary()}.
create_version(Component, Version, TmpDir) ->
    ComponentDir = component_dir(Component),
    GitDir = git_dir(ComponentDir),
    case ds_api_git:versions(GitDir) of
        {ok, Versions} ->
            %% Make sure the version actually exist
            case ordsets:is_element(Version, Versions) of
                true ->
                    ok = make_version_dir(TmpDir),
                    FilesDir = files_dir(TmpDir),
                    case ds_api_git:copy_tag(GitDir, Version, FilesDir) of
                        ok    -> ds_api_preprocess:run(Component, Version, TmpDir);
                        Error -> Error
                    end;
                false ->
                    {error, <<"Version does not exist as a tag.">>}
            end;
        Error ->
            Error
    end.

%% @doc Create the directory structure for a version directory
-spec make_version_dir(binary()) -> ok.
make_version_dir(Directory) ->
    ok = file:make_dir(files_dir(Directory)),
    ok = file:make_dir(remote_dir(Directory)),
    ok = file:make_dir(sass_dir(Directory)).

%% @doc Delete a specific version
-spec delete_version(component(), version()) -> ok.
delete_version(Component, Version) ->
    ComponentDir = component_dir(Component),
    VersionsDir = versions_dir(ComponentDir),
    VersionDir = version_dir(VersionsDir, Version),
    TmpDir = ds_api_util:tmp_path(),
    ok = file:rename(VersionDir, TmpDir),
    ds_api_util:delete_dir(TmpDir),
    ok.
