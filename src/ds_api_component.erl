-module(ds_api_component).

-export([dir/1]).
-export([git_dir/1]).
-export([versions_dir/1]).
-export([version_dir/2]).
-export([files_dir/1]).
-export([file_path/3]).
-export([list/0]).
-export([list/1]).
-export([versions/1]).
-export([diversityJSON/2]).
-export([settings/2]).
-export([settingsForm/2]).
-export([thumbnail_path/2]).
-export([expand_version/2]).

dir(Component) ->
    filename:join(ds_api:components_dir(), Component).

git_dir(ComponentDir) ->
    filename:join(ComponentDir, git).

versions_dir(ComponentDir) ->
    filename:join(ComponentDir, versions).

version_dir(VersionDir, Version) ->
    BinVersion = ds_api_version:to_binary(Version),
    filename:join(VersionDir, BinVersion).

files_dir(VersionDir) ->
    filename:join(VersionDir, files).

file_path(Component, Version, File) ->
    ComponentDir = dir(Component),
    VersionsDir = versions_dir(ComponentDir),
    VersionDir = version_dir(VersionsDir, Version),
    FilesDir = files_dir(VersionDir),
    filename:join(FilesDir, File).


list() ->
    ds_api_component_mgr:list_components().

list(Groups) ->
    lists:filter(
        fun (Component) ->
            case diversityJSON(Component, head) of
                #{<<"grouping">> := Groupings} ->
                    HasGroup = fun(Group) -> lists:member(Group, Groupings) end,
                    lists:all(HasGroup, Groups);
                _NoGroupings ->
                    false
            end
        end,
        list()
    ).

versions(Component) ->
    ds_api_component_mgr:list_versions(Component).

diversityJSON(Component, Version) ->
    ds_api_cache:get(
        {diversityJSON, Component, Version},
        fun () -> 
                FilePath = file_path(Component, Version, <<"diversity.json">>),
                {ok, File} = file:read_file(FilePath),
                jiffy:decode(File, [return_maps])
        end,
        5* 60 * 60 * 1000 %% 5 hours
     ).

settings(Component, Version) ->
    case diversityJSON(Component, Version) of
        #{<<"settings">> := Settings} -> Settings;
        _NoSettings                   -> undefined
    end.

settingsForm(Component, Version) ->
    case diversityJSON(Component, Version) of
        #{<<"settingsForm">> := SettingsForm} -> SettingsForm;
        _NoSettingsForm                       -> undefined
    end.

thumbnail_path(Component, Version) ->
    case diversityJSON(Component, Version) of
        #{<<"thumbnail">> := Thumbnail} -> file_path(Component, Version, Thumbnail);
        _NoThumbnail                    -> undefined
    end.

expand_version(Component, BinVersion) ->
    Version = case ds_api_version:to_version(BinVersion) of
                  {ok, SemVer}    -> SemVer;
                  {error, badarg} -> {'*', '*', '*'}
              end,
    Versions = versions(Component),
    ds_api_version:expand(Version, Versions).
