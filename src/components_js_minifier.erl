-module(components_js_minifier).

-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

%% Cowboy callbacks

init(_, Req, _Opts) ->
    {ok, Req, []}.

handle(Req, State) ->
    %% Proplist with one or more components to minfify
    {Components, Req1} = cowboy:qs_vals(Req),

    %% Build a map with all components like
    ComponentsWithFiles = get_files_components(Components),

    %% maps:map through the entire map and minify each file an put'em together
    minify_components_js(),
    {ok, Req, State}.

terminate(_Reason, _Req, _State) ->
    ok.

%% End %%

%% Return #{components => {tag, [files]}}
get_components_files(Components) ->



%% @doc
%% Go through each component/tag minify and concatenate result to one file.
%%    Each file must be in the order specified.
%%    Files may be a list of files or just one file.
%%    A file may be a relative in the components or a url either starting with // (assume http)
%%    Fetch external files and add to right spot
%%    If file is named .min.js do not minify just return value.
minify_components_js(_Map) ->
    ok.
