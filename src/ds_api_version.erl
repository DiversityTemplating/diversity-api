-module(ds_api_version).

-export([to_version/1]).
-export([to_binary/1]).
-export([expand/2]).
-export([to_exact_version/2]).
-export([is_exact/1]).

to_exact_version(Component, BinVersion) ->
    case to_version(BinVersion) of
        {ok, Version0} ->
            {Version1, Expanded} = expand(Component, Version0),
            {ok, Version1, Expanded};
        Error ->
            Error
    end.

expand(Component, Version0) ->
    case is_exact(Version0) of
        true ->
            {Version0, false};
        false ->
            Version1 = case ds_api_component:versions(Component) of
                           undefined -> Version0;
                           Versions  -> expand_(Version0, Versions)
                       end,
            {Version1, true}
    end.

expand_('*', Versions) ->
    find_latest_version(Versions);
expand_({'*', '*', '*'}, Versions) ->
    find_latest_version(Versions);
expand_({Major, '*', '*'}, Versions) ->
    find_latest_minor(Major, Versions);
expand_({Major, Minor, '*'}, Versions) ->
    find_latest_patch(Major, Minor, Versions);
expand_(Version, _Versions) ->
    Version.
    
find_latest_version(Versions) ->
    lists:last(Versions).

find_latest_minor(MajorA, Versions0) ->
    Versions1 = [Version || {MajorB, _, _} = Version <- Versions0, MajorA =:= MajorB],
    lists:last(Versions1).

find_latest_patch(MajorA, MinorA, Versions0) ->
    Versions1 = [Version || {MajorB, MinorB, _} = Version <- Versions0,
                            MajorA =:= MajorB, MinorA =:= MinorB],
    lists:last(Versions1).

is_exact({Major, Minor, Patch}) ->
    Major =/= '*' andalso Minor =/= '*' andalso Patch =/= '*'.

to_binary({'*', '*', '*'}) ->
    <<$*>>;
to_binary({Major, '*', '*'}) when is_integer(Major) ->
    integer_to_binary(Major);
to_binary({Major, Minor, '*'}) when is_integer(Major);
                                    is_integer(Minor) ->
    <<(integer_to_binary(Major))/binary, $.,
      (integer_to_binary(Minor))/binary>>;
to_binary({Major, Minor, Patch}) when is_integer(Major);
                                      is_integer(Minor);
                                      is_integer(Patch) ->
    <<(integer_to_binary(Major))/binary, $.,
      (integer_to_binary(Minor))/binary, $.,
      (integer_to_binary(Patch))/binary>>.

to_version(<<$*>>) ->
    {ok, {'*', '*', '*'}};
to_version(Tag) when is_binary(Tag) ->
    try
        case binary:split(Tag, <<$.>>, [global]) of
            [Major0, Minor0, Patch0] ->
                case {binary_to_integer(Major0),
                      binary_to_integer(Minor0),
                      binary_to_integer(Patch0)} of
                    {Major1, Minor1, Patch1} = Version
                      when Major1 >= 0, Minor1 >= 0, Patch1 >= 0 ->
                        {ok, Version};
                    _ ->
                        {error, badarg}
                end;
            [Major0, Minor0] ->
                case {binary_to_integer(Major0), binary_to_integer(Minor0)} of
                    {Major1, Minor1} when Major1 >= 0, Minor1 >= 0 ->
                        {ok, {Major1, Minor1, '*'}};
                    _ ->
                        {error, badarg}
                end;
            [Major0] ->
                {ok, {binary_to_integer(Major0), '*', '*'}}
        end
    catch
        error:_ -> {error, badarg}
    end.

