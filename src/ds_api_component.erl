-module(ds_api_component).

-export([path/1]).
-export([path/2]).
-export([exists/1]).
-export([exists/2]).
-export([exists/3]).
-export([list/0]).
-export([list/1]).
-export([versions/1]).
-export([diversityJSON/2]).
-export([settings/2]).
-export([settingsForm/2]).
-export([file/3]).
%-export([css/4]).
-export([thumbnail/2]).
-export([tag_to_version/1]).
-export([version_to_tag/1]).
-export([expand_tag_to_version/2]).

path(Component) ->
    filename:join(ds_api:components_dir(), Component).

path(Component, Tag) ->
    filename:join([path(Component), versions, Tag]).

path(Component, Tag, File) ->
    filename:join(path(Component, Tag), files, File).

exists(Component) ->
    filelib:is_dir(path(Component)).

exists(Component, Tag) ->
    filelib:is_dir(path(Component, Tag)).

exists(Component, Tag, File) ->
    file:is_file(path(Component, Tag, File)).

list() ->
    ds_api_component_mgr:list_components().

list(Groups) ->
    lists:filter(
        fun (Component) ->
            case diversityJSON(Component, head) of
                #{<<"grouping">> := Groupings} ->
                    HasGroup = fun(Group) -> lists:member(Group, Groupings) end,
                    lists:all(HasGroup, Groups);
                _NoGroupings ->
                    false
            end
        end,
        list()
    ).

versions(Component) ->
    ds_api_component_mgr:list_versions(Component).

diversityJSON(Component, Tag) ->
    Timeout = case Tag of
        head -> 0;
        _    -> 5* 60 * 60 * 1000 %% 5 hours
    end,
    ds_api_cache:get(
        {diversityJSON, Component, Tag},
        fun () -> 
            {ok, File} = file(Component, Tag, <<"diversity.json">>),
            jiffy:decode(File, [return_maps])
        end,
        Timeout
     ).

settings(Component, Tag) ->
    case diversityJSON(Component, Tag) of
        #{<<"settings">> := Settings} -> Settings;
        _NoSettings                   -> undefined
    end.

settingsForm(Component, Tag) ->
    case diversityJSON(Component, Tag) of
        #{<<"settingsForm">> := SettingsForm} -> SettingsForm;
        _NoSettingsForm                       -> undefined
    end.

file(Component, Tag, File) ->
    file:read_file(path(Component, Tag, File)).

%css(Component, Tag, #{styles := Styles, hash := Hash, variables := Variables}) ->
%    Output = filename:join([path(Component, Tag), sass, Hash]),
%    case filelib:is_file(Output) of
%        true  -> Output;
%        false -> sass_compile(Input, Output, Variables)
%    end.

thumbnail(Component, Tag) ->
    case diversityJSON(Component, Tag) of
        #{<<"thumbnail">> := Thumbnail} -> path(Component, Tag, Thumbnail);
        _NoThumbnail                    -> undefined
    end.

%% @doc Compile a sass-file with given variables
%sass_compile(Component, Tag, Input, Output, Variables) ->
%    %% Construct the SASS-variables
%
%    ok = filelib:ensure_dir(Output),
%    {ok, File} = file:open(Output, [append]),
%    ok = file:write(Output, BinaryVars>>),
%    {ok, SCSS} = file:read_file(Input)
%
%    %% Compile it
%    PrivDir = unicode:characters_to_binary(code:priv_dir(ds_api)),
%    SassC = filename:join(PrivDir, <<"sassc">>),
%    Command = <<SassC/binary, " -t compressed ", Output/binary>>,
%    Port = erlang:open_port({spawn, Command}, [exit_status, binary, stderr_to_stdout]),
%    Result = wait_for_response(Port, <<>>),
%
%    %% Cleanup
%    file:delete(TmpPath),
%
%    Result.

%% @doc Expand an incoming tag into the latest matching version
%% Supported formats are:
%% *     - Latest version
%% X     - Latest minor in given major
%% X.Y   - Latest patch in given major and minor
%% X.Y.Z - Given version if it is in the given Tags
%%
%% all cases will throw an error if no version can be found
expand_tag_to_version(Component, Tag) ->
    Version = case tag_to_version(Tag) of
                  {ok, SemVer}    -> SemVer;
                  {error, badarg} -> {'*', '*', '*'}
              end,
    Versions = versions(Component),
    expand_version(Version, Versions).

expand_version({'*', '*', '*'}, Versions) ->
    find_latest_version(Versions);
expand_version({Major, '*', '*'}, Versions) ->
    find_latest_minor(Major, Versions);
expand_version({Major, Minor, '*'}, Versions) ->
    find_latest_patch(Major, Minor, Versions);
expand_version(Version, Versions) ->
    case lists:member(Version, Versions) of
        %% If it exists, then it's the tag we are looking for
        true  -> Version;
        %% Otherwise default to the latest
        false -> expand_version({'*', '*', '*'}, Versions)
    end.

%% @doc Retrive the latest version according to the given comparsion function
find_latest_version(Versions) ->
    lists:last(Versions).

find_latest_minor(MajorA, Versions0) ->
    Versions1 = [Version || {MajorB, _, _} = Version <- Versions0, MajorA =:= MajorB],
    lists:last(Versions1).

find_latest_patch(MajorA, MinorA, Versions0) ->
    Versions1 = [Version || {MajorB, MinorB, _} = Version <- Versions0,
                            MajorA =:= MajorB, MinorA =:= MinorB],
    lists:last(Versions1).

%% @doc Version to binary
%% May throw error:badarg.
version_to_tag({Major, Minor, Patch}) when Major >= 0, Minor >= 0, Patch >= 0 ->
    <<(integer_to_binary(Major))/binary, $.,
      (integer_to_binary(Minor))/binary, $.,
      (integer_to_binary(Patch))/binary>>.

%% @doc Binary or list to version tuple
tag_to_version(<<$*>>) ->
    {'*', '*', '*'};
tag_to_version(Tag) when is_binary(Tag) ->
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

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

expand_tag_test_() ->
    L = lists:seq(0,3),
    Tags = [<<"invalid">> | [version_to_binary({X,Y,Z}) || X <- L, Y <- L, Z <- L]],
    [?_assertEqual(<<"3.3.3">>, expand_tag(<<"*">>, Tags)),
     ?_assertEqual(<<"1.3.3">>, expand_tag(<<"1">>, Tags)),
     ?_assertEqual(<<"2.3.3">>, expand_tag(<<"2.3">>, Tags)),
     ?_assertEqual(<<"1.2.3">>, expand_tag(<<"1.2.3">>, Tags)),
     ?_assertError(_,           expand_tag(<<"4">>, Tags)),
     ?_assertError(_,           expand_tag(<<"3.4">>, Tags)),
     ?_assertError(_,           expand_tag(<<"*">>, [<<"no_valid_tags">>]))].

-endif.
