-module(component_handler).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {
}).

init(_, Req, _Opts) ->
	{ok, Req, #state{}}.

handle(Req, State=#state{}) ->
    {PathInfo, Req2} = cowboy_req:path_info(Req),
    {ComponentName, Req3} = cowboy_req:binding(component, Req2),

    {ok, Req4} = case PathInfo of
                     [] ->
                         Tags = git_utils:tags(ComponentName),
                         cowboy_req:reply(200,
                             [{<<"content-type">>, <<"application/json">>}],
                             jiffy:encode(Tags),
                             Req3);
                     _ -> cowboy_req:reply(404, Req3)
                 end,

    {ok, Req4, State}.

terminate(_Reason, _Req, _State) ->
	ok.
