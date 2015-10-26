-module(ds_api_css).

-export([minify/1, compile/3]).

minify(CSSFile) ->
    RootName = filename:rootname(CSSFile, <<".css">>),
    MinifiedFile = <<RootName/binary, ".min.css">>,
    lager:debug(
      "MINIFYING CSS~n"
      "INPUT: ~p~n"
      "OUTPUT: ~p~n",
      [CSSFile, MinifiedFile]
     ),
    minify(CSSFile, MinifiedFile).

minify(Input, Output) ->
    SassC = sassc_path(),
    Command = <<SassC/binary, " -t compressed ", Input/binary, " ", Output/binary>>,
    WorkingDir = filename:dirname(Input),
    case ds_api_util:cmd(Command, WorkingDir) of
        {ok, _Reply} -> {ok, Output};
        Error        -> Error
    end.

%% @doc Compile a SASS-file with a given include path
compile(LoadPath, Input, Output) ->
    SassC = sassc_path(),
    Command = <<SassC/binary, " -t compressed -I ", LoadPath/binary,
                " ", Input/binary, " ", Output/binary>>,
    WorkingDir = filename:dirname(Input),
    ds_api_util:cmd(Command, WorkingDir).

sassc_path() ->
    filename:join(unicode:characters_to_binary(code:priv_dir(ds_api)), <<"sassc">>).
