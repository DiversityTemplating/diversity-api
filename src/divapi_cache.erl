-module(divapi_cache).
-behaviour(gen_server).

%% API.
-export([start_link/0, get_components/0, get_tags/1, get_diversity_json/2, get_diversity_json/3, populate_cache/0, empty_cache/1]).
%% gen_server.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    code_change/3, terminate/2]).

-record(component, {name, tag, json}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

populate_cache() ->
    gen_server:call(?MODULE, populate).

get_components() ->
    gen_server:call(?MODULE, get_components).

get_tags(Component) ->
    gen_server:call(?MODULE, {get_tags, Component}).

get_diversity_json(Component, Tag) ->
    gen_server:call(?MODULE, {get_json, Component, Tag}).

get_diversity_json(Component, ComponentUrl, Tag) ->
    gen_server:call(?MODULE, {get_json, Component, ComponentUrl, Tag}).

empty_cache(Component) ->
    gen_server:call(?MODULE, {delete, Component}).


%% gen_server.

init([]) ->
    Tab = ets:new(?MODULE, [bag, {keypos, #component.name}]),
    {ok, Tab}.

handle_call(populate, _From, Tab) ->
    populate(Tab),
    {reply, ok, Tab};

handle_call(get_components, _From, Tab) ->
    populate_if_empty(Tab),
    Names = ets:match(Tab, #component{name='$1', tag= <<"HEAD">>, json='$2'}),
    Reply = lists:usort(lists:map(fun list_to_tuple/1, Names)),
    {reply, Reply, Tab};

handle_call({get_tags, ComponentName}, _From, Tab) ->
    Reply = case ets:lookup(Tab, ComponentName) of
                [] ->
                    case ets:first(Tab) of
                        '$end_of_table' -> populate(Tab);
                        _ -> add_component(ComponentName, undefined, Tab)
                    end,
                    Select = [{#component{name = ComponentName, tag = '$1', json = '_'}, [], ['$1']}],
                    GitTags = ets:select(Tab, Select),
                    lists:subtract(GitTags, [<<"HEAD">>]);
                 ComponentRows ->
                    [ Component#component.tag || Component <- ComponentRows, Component#component.tag /= <<"HEAD">> ]

            end,
    {reply, Reply, Tab};

handle_call({get_json, ComponentName, Tag}, _From, Tab) ->
    Reply = get_json_helper(ComponentName, undefined, Tag, Tab),
    {reply, Reply, Tab};

handle_call({get_json, ComponentName, ComponentUrl, Tag}, _From, Tab) ->
    Reply = get_json_helper(ComponentName, ComponentUrl, Tag, Tab),
    {reply, Reply, Tab};

handle_call({delete, ComponentName}, _From, Tab) ->
    ets:delete(Tab, ComponentName),
    {reply, ok, Tab};

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

populate_if_empty(Tab) ->
    case ets:first(Tab) of
        '$end_of_table' -> populate(Tab);
        _ -> ok
    end.

populate(Tab) ->
    Projects = gitlab_utils:get_public_projects(),
    lists:foreach(fun({CName, CUrl}) -> add_component(CName, CUrl, Tab) end, maps:to_list(Projects)).

add_component(ComponentName, ComponentUrl, Tab) ->
    Tags = git_utils:tags(ComponentName, ComponentUrl),
    lists:foreach(fun(Tag) -> insert_new_json(ComponentName, ComponentUrl, Tag, Tab) end, [<<"HEAD">> | Tags]).

get_json_helper(ComponentName, ComponentUrl, Tag, Tab) ->
    Select = [{#component{name = ComponentName, tag = Tag, json = '$1'}, [], ['$1']}],
    case ets:select(Tab, Select) of
        [] ->
            case ets:first(Tab) of
                '$end_of_table' -> populate(Tab);
                _ -> add_component(ComponentName, ComponentUrl, Tab)
            end,
            insert_new_json(ComponentName, ComponentUrl, Tag, Tab);
        [undefined] ->
            ets:select_delete(Tab, Select),
            insert_new_json(ComponentName, ComponentUrl, Tag, Tab);
        [Json] ->
            Json
    end.

insert_new_json(ComponentName, ComponentUrl, Tag, Tab) ->
    NewJson = git_utils:get_diversity_json(ComponentName, ComponentUrl, Tag),
    NewComponent = [#component{name = ComponentName, tag = Tag, json = NewJson}],
    ets:insert(Tab, NewComponent),
    NewJson.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal
