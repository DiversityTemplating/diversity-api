-module(ds_api_version).

-export([to_version/1]).
-export([to_binary/1]).
-export([expand/2]).
-export([is_specific/1]).

expand({'*', '*', '*'}, Versions) ->
    find_latest_version(Versions);
expand({Major, '*', '*'}, Versions) ->
    find_latest_minor(Major, Versions);
expand({Major, Minor, '*'}, Versions) ->
    find_latest_patch(Major, Minor, Versions);
expand(Version, Versions) ->
    case lists:member(Version, Versions) of
        %% If it exists, then it's the tag we are looking for
        true  -> Version;
        %% Otherwise default to the latest
        false -> expand({'*', '*', '*'}, Versions)
    end.

find_latest_version(Versions) ->
    lists:last(Versions).

find_latest_minor(MajorA, Versions0) ->
    Versions1 = [Version || {MajorB, _, _} = Version <- Versions0, MajorA =:= MajorB],
    lists:last(Versions1).

find_latest_patch(MajorA, MinorA, Versions0) ->
    Versions1 = [Version || {MajorB, MinorB, _} = Version <- Versions0,
                            MajorA =:= MajorB, MinorA =:= MinorB],
    lists:last(Versions1).

is_specific({Major, Minor, Patch}) ->
    Major =/= '*' andalso Minor =/= '*' andalso Patch =/= '*'.

to_binary({Major, Minor, Patch}) ->
    <<(integer_to_binary(Major))/binary, $.,
      (integer_to_binary(Minor))/binary, $.,
      (integer_to_binary(Patch))/binary>>.

to_version(<<$*>>) ->
    {'*', '*', '*'};
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

