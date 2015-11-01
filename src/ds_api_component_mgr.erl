-module(ds_api_component_mgr).

-behaviour(gen_server).

-export([start_link/0]).
-export([components/0]).
-export([versions/1]).
-export([exists/1]).
-export([exists/2]).
-export([published/2]).
-export([add_component/2]).
-export([delete_component/1]).
-export([add_version/2]).
-export([delete_version/2]).
-export([unpublished_versions/0]).
-export([publish/1]).
-export([publish/2]).

-export([diversity_json/2]).

-export([component_dir/1]).
-export([versions_dir/1]).
-export([git_dir/1]).
-export([version_dir/2]).
-export([files_dir/2]).
-export([remote_dir/2]).
-export([sass_dir/2]).
-export([temp_dir/2]).
-export([file_path/3]).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-define(TAB,  components_ets).

%% The ETS-table keeps all information about all currently loaded components and versions.
%% There are three types of entries:
%% {components, Components :: ordsets:ordset(Component :: binary())
%% {{component, Component :: binary()}, , Versions :: ordsets:ordset(Version :: ds_api_version:version())}
%% {{version, Component :: binary(), Version :: ds_api_version:version()}, Published :: boolean()}

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

components() ->
    [{components, Components}] = ets:lookup(?TAB, components),
    Components.

versions(Component) ->
    Key = {component, Component},
    case ets:lookup(?TAB, Key) of
        [{Key, Versions}] -> Versions;
        _Undefined        -> undefined
    end.

exists(Component) ->
    ets:member(?TAB, {component, Component}).

exists(Component, Version) ->
    ets:member(?TAB, {version, Component, Version}).

published(Component, Version) ->
    case ets:lookup(?TAB, {version, Component, Version}) of
        [{_, Published, _Diversity}] -> Published;
        _Undefined                   -> undefined
    end.

add_component(Component, RepoURL) ->
    Timeout = 5 * 60 * 1000,
    gen_server:call(?MODULE, {add_component, Component, RepoURL}, Timeout).

delete_component(Component) ->
    Timeout = 5 * 60 * 1000,
    gen_server:call(?MODULE, {delete_component, Component}, Timeout).

add_version(Component, Version) ->
    Timeout = 5 * 60 * 1000,
    gen_server:call(?MODULE, {add_version, Component, Version}, Timeout).

delete_version(Component, Version) ->
    Timeout = 5 * 60 * 1000,
    gen_server:call(?MODULE, {delete_version, Component, Version}, Timeout).

unpublished_versions() ->
    gen_server:call(?MODULE, unpublished_versions).

publish(ComponentVersions) when is_map(ComponentVersions) ->
    gen_server:call(?MODULE, {publish, ComponentVersions}).

publish(Component, Version) ->
    publish(maps:put(Component, [Version], #{})).

diversity_json(Component, Version) ->
    case ets:lookup(?TAB, {version, Component, Version}) of
        [{{version, Component, Version}, _Published, Diversity}] -> {ok, Diversity};
        _Undefined                                               -> undefined
    end.

component_dir(Component) ->
    ComponentsDir = ds_api:components_dir(),
    filename:join(ComponentsDir, Component).

versions_dir(Component) ->
    filename:join(component_dir(Component), versions).

git_dir(Component) ->
    filename:join(component_dir(Component), git).

version_dir(Component, Version) ->
    filename:join(versions_dir(Component), ds_api_version:to_binary(Version)).

files_dir(Component, Version) ->
    filename:join(version_dir(Component, Version), files).

remote_dir(Component, Version) ->
    filename:join(version_dir(Component, Version), remote).

sass_dir(Component, Version) ->
    filename:join(version_dir(Component, Version), sass).

temp_dir(Component, Version) ->
    Temp = integer_to_binary(erlang:phash2(erlang:make_ref())),
    filename:join([version_dir(Component, Version), temp, Temp]).

file_path(Component, Version, File) ->
    filename:join(files_dir(Component, Version), File).

init(_Config) ->
    bootstrap_components(),

    {ok, no_state}.

handle_call({add_component, Component, RepoURL}, _From, State) ->
    {reply, do_add_component(Component, RepoURL), State};
handle_call({delete_component, Component}, _From, State) ->
    {reply, do_delete_component(Component), State};
handle_call({add_version, Component, Version}, _From, State) ->
    {reply, do_add_version(Component, Version), State};
handle_call({delete_version, Component, Version}, _From, State) ->
    {reply, do_delete_version(Component, Version), State};
handle_call({publish, ComponentVersions}, _From, State) ->
    {reply, do_publish_versions(ComponentVersions), State};
handle_call(unpublished_versions, _From, State) ->
    {reply, get_unpublished_versions(), State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

do_add_component(Component, RepoURL) ->
    case exists(Component) of
        true ->
            {error, component_exists};
        false ->
            case do_create_component(Component, RepoURL) of
                ok ->
                    Components0 = components(),
                    Components1 = ordsets:add_element(Component, Components0),
                    ComponentsEntry = {components, Components1},
                    ComponentEntry = {{component, Component}, ordsets:new()},
                    true = ets:insert(?TAB, [ComponentsEntry, ComponentEntry]),
                    ok;
                Error ->
                    ds_api_util:delete_dir(component_dir(Component)),
                    Error
            end
    end.

do_create_component(Component, RepoURL) ->
    GitDir = git_dir(Component),
    VersionsDir = versions_dir(Component),
    case filelib:ensure_dir(VersionsDir) of
        ok    -> ds_api_git:clone(RepoURL, GitDir);
        Error -> Error
    end.

do_delete_component(Component) ->
    %% Remove component from the components entry
    Components0 = components(),
    Components1 = ordsets:del_element(Component, Components0),
    true = ets:insert(?TAB, {components, Components1}),

    %% Remove the component entry
    true = ets:delete(?TAB, {component, Component}),

    %% Remove all version entries
    true = ets:match_delete(?TAB, {{version, Component, '_'}, '_', '_'}),

    %% Delete the component directory
    ComponentDir = component_dir(Component),
    ds_api_util:delete_dir(ComponentDir),
    ok.

do_add_version(Component, Version) ->
    case exists(Component, Version) of
        true ->
            {error, exists};
        false ->
            case do_create_version(Component, Version) of
                {ok, Diversity} ->
                    %% Create an unpublished entry
                    VersionEntry = {{version, Component, Version}, false, Diversity},
                    true = ets:insert(?TAB, VersionEntry),
                    ok;
                Error ->
                    ds_api_util:delete_dir(version_dir(Component, Version)),
                    Error
            end
    end.

do_create_version(Component, Version) ->
    VersionBin = ds_api_version:to_binary(Version),
    GitDir = git_dir(Component),
    FilesDir = files_dir(Component, Version),
    case get_git_versions(GitDir) of
        {ok, Versions} ->
            case ordsets:is_element(Version, Versions) of
                true ->
                    case ds_api_git:copy_tag(GitDir, VersionBin, FilesDir) of
                        ok    -> ds_api_preprocess:run(Component, Version);
                        Error -> Error
                    end;
                false ->
                    {error, unknown_version}
            end;
        Error ->
            Error
    end.

do_delete_version(Component, Version) ->
    VersionDir = version_dir(Component, Version),
    case versions(Component) of
        undefined ->
            ok;
        Versions0 ->
            Versions1 = ordsets:del_element(Version, Versions0),
            ets:insert(?TAB, {{component, Component}, Versions1})
    end,
    ets:delete(?TAB, {version, Component, Version}),
    ds_api_util:delete_dir(VersionDir),
    ok.

do_publish_versions(ComponentVersions) ->
    case check_all_unpublished(ComponentVersions) of
        ok ->
            UpdatedEntries = maps:fold(fun get_version_table_updates/3, [], ComponentVersions),
            true = ets:insert(?TAB, UpdatedEntries),
            ok;
        Error ->
            Error
    end.

get_version_table_updates(Component, NewVersions, Acc) ->
    Versions0 = versions(Component),
    Versions1 = ordsets:union(Versions0, ordsets:from_list(NewVersions)),
    ComponentEntry = {{component, Component}, Versions1},
    PublishedVersionEntries = get_published_version_entries(Component, NewVersions),
    [ComponentEntry | PublishedVersionEntries] ++ Acc.

get_published_version_entries(Component, NewVersions) ->
    [begin
         Key = {version, Component, Version},
         [{Key, false, Diversity}] = ets:lookup(?TAB, Key),
         {Key, true, Diversity}
     end || Version <- NewVersions].

check_all_unpublished(ComponentVersions) when is_map(ComponentVersions) ->
    check_all_unpublished(maps:to_list(ComponentVersions));
check_all_unpublished([]) ->
    ok;
check_all_unpublished([{Component, Versions} | ComponentVersions]) ->
    case check_unpublished(Component, Versions) of
        ok    -> check_all_unpublished(ComponentVersions);
        Error -> Error
    end.

check_unpublished(_Component, []) ->
    ok;
check_unpublished(Component, [Version | Versions]) ->
    case published(Component, Version) of
        false    -> check_unpublished(Component, Versions);
        _Invalid -> {error, {not_added, Component, Version}}
    end.

get_unpublished_versions() ->
    UnpublishedVersions = ets:match(?TAB, {{version, '$1', '$2'}, false, '_'}),
    ordsets:from_list([{Component, Version} || [Component, Version] <- UnpublishedVersions]).

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
        _Error ->
            {error, could_not_fetch_tags}
    end.

bootstrap_components() ->
    ?TAB = ets:new(?TAB, [{read_concurrency, true}, named_table]),
    Components = list_dir(ds_api:components_dir()),
    ComponentEntries = lists:flatmap(fun bootstrap_component/1, Components),
    true = ets:insert(?TAB, [{components, Components} | ComponentEntries]).

bootstrap_component(Component) ->
    VersionsDir = versions_dir(Component),
    Versions0 = [begin
                     {ok, Version} = ds_api_version:to_version(BinVersion),
                     Version
                 end || BinVersion <- list_dir(VersionsDir)],
    Versions1 = ordsets:from_list(Versions0),
    VersionEntries = [begin
                          DiversityPath = filename:join(files_dir(Component, Version), <<"diversity.json">>),
                          {ok, Diversity0} = file:read_file(DiversityPath),
                          Diversity1 = jiffy:decode(Diversity0, [return_maps]),
                          {{version, Component, Version}, true, Diversity1}
                      end || Version <- Versions1],
    [{{component, Component}, Versions1} | VersionEntries].

list_dir(Dir) ->
    {ok, Files} = file:list_dir(Dir),
    [unicode:characters_to_binary(filename:basename(File)) || File <- Files].
