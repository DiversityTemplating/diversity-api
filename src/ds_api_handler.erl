-module(ds_api_handler).

-export([init/3]).
-export([rest_init/2]).
-export([allowed_methods/2]).
-export([resource_exists/2]).
-export([is_authorized/2]).
-export([is_conflict/2]).
-export([content_types_provided/2]).
-export([to_json/2]).
-export([content_types_accepted/2]).
-export([create_component/2]).
-export([update_component/2]).

-record(state, {
          action,
          component,
          repo_url
         }).

init(_Type, Req, [Action]) ->
    {upgrade, protocol, cowboy_rest, Req, #state{action = Action}}.

rest_init(Req0, State) ->
    {Component, Req1} = cowboy_req:binding(component, Req0),
    {ok, Req1, State#state{component = Component}}.

allowed_methods(Req, #state{action = list} = State) ->
    {[<<"GET">>, <<"HEAD">>, <<"OPTIONS">>], Req, State};
allowed_methods(Req, #state{action = component} = State) ->
    {[<<"PUT">>, <<"GET">>, <<"HEAD">>, <<"OPTIONS">>], Req, State};
allowed_methods(Req, #state{action = update} = State) ->
    {[<<"POST">>, <<"OPTIONS">>], Req, State}.

resource_exists(Req, #state{action = list} = State) ->
    {true, Req, State};
resource_exists(Req, #state{component = Component} = State) ->
    {ds_api_component:exists(Component), Req, State}.


content_types_provided(Req, State) ->
    {[{<<"application/json">>, to_json}], Req, State}.

to_json(Req0, #state{action = list} = State) ->
    {Components, Req1} = case cowboy_req:qs_val(<<"grouping">>, Req0) of
        {undefined, Req} ->
            {ds_api_component:list(), Req};
        {GroupsBin, Req} ->
            Groups = binary:split(GroupsBin, <<$,>>, [global, trim]),
            {ds_api_component:list(Groups), Req}
    end,
    {jiffy:encode(Components), Req1, State};
to_json(Req, #state{component = Component} = State) ->
    {jiffy:encode(ds_api_component:tags(Component)), Req, State}.


