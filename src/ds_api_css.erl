-module(ds_api_css).

-export([minify/1, compile/3]).

minify(CSSFile) ->
    RootName = filename:rootname(CSSFile, <<".css">>),
    MinifiedFile = <<RootName/binary, ".min.css">>,
    minify(CSSFile, MinifiedFile).

minify(Input, Output) ->
    SassC = sassc_path(),
    Command = <<SassC/binary, " -t compressed ", Input/binary, " ", Output/binary>>,
    WorkingDir = filename:dirname(Input),
    ds_api_util:cmd(Command, WorkingDir).

%% @doc Compile a SASS-file with a given include path
compile(LoadPath, Input, Output) ->
    SassC = sassc_path(),
    Command = <<SassC/binary, " -t compressed -I ", LoadPath/binary,
                " ", Input/binary, " ", Output/binary>>,
    WorkingDir = filename:dirname(Input),
    ds_api_util:cmd(Command, WorkingDir).

sassc_path() ->
    PrivDir = unicode:characters_to_binary(code:priv_dir(ds_api)),
    filename:join(PrivDir, <<"sassc">>).
