-module(ds_api_component).

-export([components/0]).
-export([versions/1]).

-export([diversity_json/2]).
-export([diversity_json_files/3]).

-export([component_dir/1]).
-export([versions_dir/1]).
-export([version_dir/2]).
-export([file_path/2]).

-export([add_component/2]).
-export([delete_component/1]).

-export([add_version/2]).
-export([delete_version/2]).

components() ->
    ComponentsDir = ds_api:config(components_dir),
    {ok, Components} = file:list_dir(ComponentsDir),
    lists:sort([unicode:characters_to_binary(Component) || Component <- Components]).

versions(Component) ->
    VersionsDir = versions_dir(Component),
    {ok, Versions0} = file:list_dir(VersionsDir),
    Versions1 = [unicode:characters_to_binary(Version) || Version <- Versions0],
    Versions2 = lists:filtermap(
                  fun (Version0) ->
                          case ds_api_version:to_version(Version0) of
                              {ok, Version1}  -> {true, Version1};
                              {error, badarg} -> false
                          end
                  end,
                  Versions1
                 ),
    lists:sort(Versions2).

diversity_json(Component, Version) ->
    ds_api_cache:get(
      {diversity_json, Component, Version},
      fun () ->
              VersionDir = version_dir(Component, Version),
              DiversityPath = filename:join(VersionDir, <<"files/diversity.json">>),
              case file:read_file(DiversityPath) of
                  {ok, Diversity} ->
                      {ok, jiffy:decode(Diversity)};
                  _Undefined ->
                      undefined
              end
      end,
      60 * 1000
     ).

diversity_json_files(Property, Diversity, Directory) ->
    Files = case maps:find(Property, Diversity) of
                {ok, F} when is_binary(F) -> [F];
                {ok, Fs} when is_list(Fs) -> Fs;
                error                     -> []
            end,
    lists:flatmap(
      fun (<<"//", _/binary>> = Remote) ->
              [<<"http:", Remote/binary>>];
          (<<"http://", _/binary>> = Remote) ->
              [Remote];
          (<<"https://", _/binary>> = Remote) ->
              [Remote];
          (File0) ->
              File1 = filename:join([Directory, files, File0]),
              File2 = unicode:characters_to_list(File1),
              [unicode:characters_to_binary(File) || File <- filelib:wildcard(File2)]
      end,
      Files
     ).

component_dir(Component) ->
    ComponentsDir = ds_api:components_dir(),
    filename:join(ComponentsDir, Component).

versions_dir(Component) ->
    filename:join(component_dir(Component), versions).

version_dir(Component, Version) ->
    filename:join(versions_dir(Component), ds_api_version:to_binary(Version)).

file_path(<<"http://", URL/binary>>, Directory)  -> filename:join([Directory, remote, http, URL]);
file_path(<<"https://", URL/binary>>, Directory) -> filename:join([Directory, remote, https, URL]);
file_path(LocalFile, Directory)                  -> filename:join(Directory, LocalFile).

add_component(Component, RepoURL) ->
    ComponentDir = ds_api_component:component_dir(Component),

    TmpDir = ds_api_util:tmp_dir(),
    ok = file:make_dir(TmpDir),

    VersionsDir = filename:join(TmpDir, versions),
    ok = file:make_dir(VersionsDir),

    GitDir = filename:join(TmpDir, git),
    case ds_api_git:clone(RepoURL, GitDir) of
        ok ->
            filename:rename(TmpDir, ComponentDir);
        Error ->
            ok = ds_api_util:delete_dir(TmpDir),
            Error
    end.

delete_component(Component) ->
    ComponentDir = ds_api_component:component_dir(Component),
    TmpDir = ds_api_util:tmp_dir(),
    ok = file:rename(ComponentDir, TmpDir),
    ok = ds_api_util:delete_dir(TmpDir).

add_version(Component, Version) ->
    VersionDir = ds_api_component:version_dir(Component, Version),
    TmpDir = ds_api_util:tmp_dir(),
    case create_version(Component, Version, TmpDir) of
        ok ->
            filename:rename(TmpDir, VersionDir);
        Error ->
            ok = ds_api_util:delete_dir(TmpDir),
            Error
    end.

create_version(Component, Version, TmpDir) ->
    GitDir = filename:join(component_dir(Component), git),
    case get_git_versions(GitDir) of
        {ok, Versions} ->
            case ordsets:is_element(Version, Versions) of
                true ->
                    FilesDir = filename:join(TmpDir, files),
                    VersionBin = ds_api_version:to_binary(Version),
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

delete_version(Component, Version) ->
    VersionDir = ds_api_component:version_dir(Component, Version),
    TmpDir = ds_api_util:tmp_dir(),
    ok = file:rename(VersionDir, TmpDir),
    ok = ds_api_util:delete_dir(TmpDir).

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
