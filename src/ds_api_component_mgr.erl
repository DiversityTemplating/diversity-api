-module(ds_api_component_mgr).

-behaviour(gen_server).

-export([start_link/0]).
-export([components/0]).
-export([versions/1]).
-export([exists/1]).
-export([exists/2]).
-export([add/2]).
-export([update/1]).
-export([publish/1]).
-export([staged/0]).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-define(TAB, ?MODULE).

-type version() :: {non_neg_integer(), non_neg_integer(), non_neg_integer()}.
-type versions() :: ordsets:ordset(version()).

-record(state, {
          staged = #{} :: #{binary() => versions()}
         }).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

components() ->
    [{components, Components}] = ets:lookup(?TAB, components),
    Components.

versions(Component) ->
    Key = {component, Component},
    case ets:lookup(?TAB, Key) of
        [{Key, Versions}] -> Versions;
        _Error            -> undefined
    end.

exists(Component) ->
    versions(Component) =/= undefined.

exists(Component, Version) ->
    case versions(Component) of
        undefined -> false;
        Versions  -> lists:member(Version, Versions)
    end.

add(Component, RepoURL) ->
    Timeout = 5 * 60 * 1000,
    gen_server:call(?MODULE, {add, Component, RepoURL}, Timeout).

update(Component) ->
    Timeout = 5 * 60 * 1000,
    gen_server:call(?MODULE, {update, Component}, Timeout).

publish(Component) ->
    gen_server:call(?MODULE, {publish, Component}).

staged() ->
    gen_server:call(?MODULE, staged).


init(_Config) ->
    ?TAB = ets:new(?TAB, [{read_concurrency, true}, named_table]),
    true = ets:insert(?TAB, {components, ordsets:new()}),
    {ok, #state{}}.

handle_call({add, Component, RepoURL}, _From, #state{staged = Staged0} = State) ->
    case exists(Component) of
        true ->
            {reply, {error, exists}, State};
        false ->
            case maps:is_key(Component, Staged0) of
                true ->
                    {reply, {error, staged}, State};
                false ->
                    case add_component(Component, RepoURL) of
                        {ok, Changes} ->
                            Staged1 = maps:put(Component, Changes, Staged0),
                            {reply, ok, State#state{staged = Staged1}};
                        Error ->
                            {reply, Error, State}
                    end
            end
    end;
handle_call({update, Component}, _From, #state{staged = Staged0} = State) ->
    case not exists(Component) of
        true ->
            {reply, {error, unknown}, State};
        false ->
            case maps:is_key(Component, Staged0) of
                true ->
                    {reply, {error, staged}, State};
                false ->
                    case update_component(Component) of
                        {ok, Changes} ->
                            Stage1 = maps:put(Component, Changes, Staged0),
                            {reply, ok, State#state{staged = Stage1}};
                        Error ->
                            {reply, Error, State}
                    end
            end
    end;
handle_call({publish, Component}, _From, #state{staged = Staged0} = State) ->
    case maps:is_key(Component, Staged0) of
        true ->
            Staged1 = publish_component(Component, Staged0),
            {reply, ok, State#state{staged = Staged1}};
        false ->
            {error, not_staged}
    end;
handle_call(staged, _From, #state{staged = Staged} = State) ->
{reply, Staged, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

add_component(Component, RepoURL) ->
    ComponentDir = ds_api_component:dir(Component),
    VersionsDir = ds_api_component:versions_dir(ComponentDir),
    case filelib:ensure_dir(ComponentDir) of
        ok ->
            case filelib:ensure_dir(VersionsDir) of
                ok ->
                    case ds_api_git:clone(RepoURL, ComponentDir) of
                        ok    -> update_component(Component);
                        Error -> Error
                    end;
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

update_component(Component) ->
    Versions = versions(Component),
    ComponentDir = ds_api_component:dir(Component),
    GitDir = ds_api_component:git_dir(ComponentDir),
    case ds_api_git:tags(GitDir) of
        {ok, Tags} ->
            GitVersions0 = lists:filtermap(
                             fun (Tag) ->
                                     case ds_api_version:to_version(Tag) of
                                         {ok, Version} -> {true, Version};
                                         {error, badarg} -> false
                                     end
                             end,
                             Tags
                            ),
            GitVersions1 = ordsets:from_list(GitVersions0),
            AddedVersions = ordsets:subtract(GitVersions1, Versions),
            RemovedVersions = ordsets:substract(Versions, GitVersions1),
            case add_versions(Component, AddedVersions) of
                ok    -> {ok, {AddedVersions, RemovedVersions}};
                Error -> Error
            end;
        Error ->
            Error
    end.

add_versions(_ComponentDir, []) ->
    ok;
add_versions(Component, [Version | Versions]) ->
    case add_version(Component, Version) of
        ok    -> add_versions(Component, Versions);
        Error -> Error
    end.

add_version(Component, Version) ->
    ComponentDir = ds_api_component:dir(Component),
    GitDir = ds_api_component:git_dir(ComponentDir),
    VersionsDir = ds_api_component:versions_dir(ComponentDir),
    VersionDir = ds_api_component:version_dir(VersionsDir, Version),
    Tag = ds_api_version:to_binary(Version),
    case filelib:ensure_dir(VersionDir) of
        ok ->
            FilesDir = ds_api_component:files_dir(VersionDir),
            case ds_api_git:copy_tag(GitDir, Tag, FilesDir) of
                ok    -> ds_api_preprocess:run(Component, Version, VersionDir);
                Error -> Error
            end;
        Error ->
            Error
    end.

publish_component(Component, Staged) ->
    %% Create new components
    Components0 = components(),
    Components1 = ordsets:add_element(Component, Components0),

    %% Update the components versions
    Versions0 = versions(Component),
    {NewVersions, RemovedVersions} = maps:get(Component, Staged),
    Versions1 = ordsets:union(Versions0, NewVersions),
    Versions2 = ordsets:subtract(Versions1, RemovedVersions),

    %% Execute changes
    Entries = [{components, Components1}, {component, Component, Versions2}],
    ok = ets:insert(?TAB, Entries),

    %% Remove unneeded version
    ComponentsDir = ds_api_component:dir(Component),
    VersionsDir = ds_api_component:versions_dir(ComponentsDir),
    lists:foreach(
      fun (Version) -> delete_version_dir(VersionsDir, Version) end,
      RemovedVersions
     ).

delete_version_dir(VersionsDir, Version) ->
    VersionDir = ds_api_component:version_dir(VersionsDir, Version),
    try
        [] = os:cmd("rm -r " ++ unicode:characters_to_list(VersionDir))
    catch
        error:badmatch ->
            %% TODO: Log to sentry
            ok
    end.
