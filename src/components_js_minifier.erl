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
    {Components, Req1} = cowboy_req:qs_vals(Req),

    %% Build a map with all components like
    ScriptFilesMap = get_file_components(Components),
    io:format("~p", [ScriptFilesMap]),
    %% maps:map through the entire map and minify each file an put'em together
    Result = minify_components_js(ScriptFilesMap),
    Result,
    cowboy_req:reply(200, [], <<"FOOO">>, Req1),
    {ok, Req, State}.

terminate(_Reason, _Req, _State) ->
    ok.

%% End %%

get_file_components(Components) ->
    get_components_files(Components, #{}).

get_components_files([{Component, Tag} | Rest], Map) ->
    {DiversityProps} = jiffy:decode(git_utils:get_diversity_json(Component, Tag)),
    ScriptFiles = proplists:get_value(<<"script">>, DiversityProps),
    TaggedScriptFiles = lists:map(fun get_file_type/1, ScriptFiles),
    get_components_files(Rest, maps:put({Component, Tag}, TaggedScriptFiles, Map));
get_components_files([], Map) ->
    Map.

get_file_type(<<"\\", _Rest/binary>> = ScriptFile) ->
    %% This is an exteral js file that needs to be downloaded.
    {external, ScriptFile};
get_file_type(ScriptFile) ->
    case re:run(ScriptFile, <<".min.js">>) of
        {match, _} ->
            %% It's already minified. And exists in our component already
            {already_minified, ScriptFile};
        _ ->
            %% It's not a minified file. Assume internal
            {file, ScriptFile}
    end.

%% @doc
%% Go through each component/tag minify and concatenate result to one file.
%%    Each file must be in the order specified.
%%    Files may be a list of files or just one file.
%%    A file may be a relative in the components or a url either starting with // (assume http)
%%    Fetch external files and add to right spot
%%    If file is named .min.js do not minify just return value.
minify_components_js(_Map) ->
    ok.
