-module(ds_api_js).

-define(DEFAULT_MINIFY_TIMEOUT, 60000).

-export([minify/1]).

minify(JSFile) ->
    MinifiedFile = minified_name(JSFile),
    minify(JSFile, MinifiedFile).

minify(Input, Output) ->
    UglifyJS = uglifyjs_path(),
    Args = [<<"--screw-ie8">>, <<"-c">>, <<"-m">>, <<"-o">>, Output, Input],
    WorkingDir = filename:dirname(Input),
    case ds_api_util:cmd(UglifyJS, [{cd, WorkingDir}, {args, Args}]) of
        {ok, _Reply} -> {ok, Output};
        Error        -> Error
    end.

minified_name(JSFile) ->
    ds_api_util:minified_name(JSFile, <<".js">>).

uglifyjs_path() ->
    unicode:characters_to_binary(os:find_executable("uglifyjs")).
