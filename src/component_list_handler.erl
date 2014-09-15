-module(component_list_handler).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {
}).

init(_, Req, _Opts) ->
	{ok, Req, #state{}}.

handle(Req, State=#state{}) ->
    {Path, Req2} = cowboy_req:path(Req),
    {ok, Req3} = case Path of
        <<"/components/">> ->
            Projects = gitlab_utils:get_public_projects(),
            cowboy_req:reply(200,
                             [{<<"content-type">>, <<"application/json">>}],
                             jiffy:encode(maps:keys(Projects)),
                             Req2);
        _ -> cowboy_req:reply(404, Req2)
    end,
	{ok, Req3, State}.

terminate(_Reason, _Req, _State) ->
	ok.
