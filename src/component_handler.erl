-module(component_handler).

-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {}).

-define(JSON_HEADER, [{<<"content-type">>, <<"application/json">>}]).
-define(CSS_HEADER, [{<<"content-type">>, <<"text/css">>}]).
-define(ACCESS_CONTROL_HEADER, [{<<"Access-Control-Allow-Origin">>, <<"*">>}]).

init(_, Req, _Opts) ->
    {ok, Req, #state{}}.

handle(Req, State=#state{}) ->
    {Method, Req2} = cowboy_req:method(Req),
    {ok, Req3} =
        case Method of
            <<"GET">> -> handle_get(Req2, State);
            _         -> reply_from_result_data(Req2, resource_not_found)
        end,
    {ok, Req3, State}.

%% @doc Handle a GET-request
handle_get(Req, _State=#state{}) ->
    {ComponentName, Req1} = cowboy_req:binding(component, Req),
    case application:get_env(divapi, stage) of
        undefined ->
            case divapi_component:exists(ComponentName) of
                true ->
                    handle_component_request(Req1, divapi_component:dir(ComponentName));
                false ->
                    reply_from_result_data(Req1, resource_not_found)
            end;
        {ok, StageEnv} ->
            handle_stage_request(Req, ComponentName, StageEnv)
    end.

handle_stage_request(Req, ComponentName, StageEnv) ->
    %% Get the subdomain for staging env as key for checking component.
    {Host, Req1} = cowboy_req:host(Req),
    Stage = case proplists:get_value(staging_regexp, StageEnv) of
        undefined ->
            <<>>;
        StagingRegExp ->
            case re:run(Host, StagingRegExp) of
                {match, [{Pos, _Length}]} ->
                    {_, SplittedUrl} = split_binary(Host, Pos),
                    [StagingName, _] = binary:split(SplittedUrl, <<".">>),
                    StagingName
            end
    end,
    case divapi_component:exists(ComponentName, Stage) of
        true ->
            handle_component_request(Req1, divapi_component:dir(ComponentName, Stage));
        false ->
            %% Do a fallback call!!
            FallBackUrl = proplists:get_value(fallback, StageEnv),
            Opts = [{body_format, binary}],
            {Path, Req2} = cowboy_req:path(Req1),
            {Qs, Req3} = cowboy_req:qs(Req2),
            Url = <<"http://", FallBackUrl/binary, Path/binary, "?", Qs/binary>>,
            Request = {unicode:characters_to_list(Url), []},
            Result = case httpc:request(get, Request, [], Opts) of
                {ok, {{_Version, Status, _ReasonPhrase}, Headers, Body}} ->
                    case Status of
                        404 -> resource_not_found;
                        500 -> {error, <<"Failed to fetch from fallback">>};
                        200 -> {ok, Body, Headers}
                    end
            end,
            case Result of
                {ok, ReplyBody, ReplyHeaders} ->
                    ContentType = proplists:get_value("content-type", ReplyHeaders),
                    Headers0 = [{<<"content-type">>, unicode:characters_to_binary(ContentType)}]
                              ++ ?ACCESS_CONTROL_HEADER,
                    cowboy_req:reply(200, Headers0, ReplyBody, Req);
                _ ->
                    reply_from_result_data(Req3, Result)
            end
    end.


%% @doc Handle a component-request
%% The possible routes to this function is:
%% /ComponentName/                    - List all tags
%% /ComponentName/Tag/                - Serve diversity.json
%% /ComponentName/Tag/files/some.file - Serve a file
%% /ComponentName/Tag/css             - Serve concatenated CSS
%% /ComponentName/Tag/thumbnail       - Serve thumbnail
handle_component_request(Req, ComponentPath) ->
    {PathInfo, Req1} = cowboy_req:path_info(Req),
    ResultData = case PathInfo of
        undefined ->
            {json, jiffy:encode(divapi_component:tags(ComponentPath))};
        [PartialTag | Routes] ->
            %% Try to expand the tag into an existing one
            try expand_tag(PartialTag, divapi_component:tags(ComponentPath)) of
                Tag ->
                    case Routes of
                        [] ->
                            {json,
                             jiffy:encode(divapi_component:diversity_json(ComponentPath, Tag))};
                        [Settings] when Settings =:= <<"settings">> ->
                            {json, jiffy:encode(divapi_component:settings(ComponentPath, Tag))};
                        [Settings] when Settings =:= <<"settingsForm">> ->
                            {json, jiffy:encode(divapi_component:settingsform(Tag, Settings))};
                        [<<"files">> | Path] ->
                            %% Both Files and thumbnails are acting weird. Streamline and make sure they look better.
                            File = filename:join(Path),
                            {BrowserEtag, _} = cowboy_req:header(<<"if-none-match">>, Req1),
                            Etag = case divapi_app:is_production() of
                                false -> <<>>;
                                true  -> <<Tag/binary, File/binary>>
                            end,
                            case BrowserEtag of
                                Etag ->
                                    file_not_changed;
                                _    ->
                                    {Mime, Type, []} = cow_mimetypes:all(File),
                                    Headers =
                                        [{<<"content-type">>, << Mime/binary, "/", Type/binary >>},
                                         {<<"Cache-Control">>, <<"max-age=90000">>},
                                         {<<"Etag">>, <<Tag/binary, File/binary>>}],
                                    case divapi_component:file(ComponentPath, Tag, File) of
                                        FileBin when is_binary(FileBin) ->
                                            {ok, {Headers, FileBin}};
                                        FileFailure ->
                                            FileFailure
                                    end
                            end;
                        [<<"css">>] ->
                            %% Retrive additional variables and sort them to get a
                            %% consistent cache key
                            {Variables, _Req2} = cowboy_req:qs_vals(Req1),
                            {css, divapi_component:css(ComponentPath, Tag, Variables)};
                        [<<"thumbnail">>] ->
                            {BrowserEtag, _} = cowboy_req:header(<<"if-none-match">>, Req1),
                            Etag = case divapi_app:is_production() of
                                false -> <<>>;
                                true  -> <<"css*", ComponentPath/binary>>
                            end,
                            case BrowserEtag of
                                Etag -> file_not_changed;
                                _    -> divapi_component:thumbnail(ComponentPath)
                            end;
                        _ ->
                            resource_not_found
                    end
            catch
                error:_ ->
                    resource_not_found
            end;
        _ ->
            resource_not_found
    end,
    reply_from_result_data(Req1, ResultData).

%% @doc Contstruct the cowboy-reply
reply_from_result_data(Req, file_not_changed) ->
    cowboy_req:reply(304, ?ACCESS_CONTROL_HEADER, Req);
reply_from_result_data(Req, resource_not_found) ->
    cowboy_req:reply(404, Req);
reply_from_result_data(Req, {error, Reason}) ->
    cowboy_req:reply(500, ?ACCESS_CONTROL_HEADER, Reason, Req);
reply_from_result_data(Req, {ok, {Headers, Data}}) ->
    cowboy_req:reply(200, ?ACCESS_CONTROL_HEADER ++ Headers, Data, Req);
reply_from_result_data(Req, {json, Data}) ->
    cowboy_req:reply(200, ?ACCESS_CONTROL_HEADER ++ ?JSON_HEADER, Data, Req);
reply_from_result_data(Req, {css, Data}) ->
    cowboy_req:reply(200, ?ACCESS_CONTROL_HEADER ++ ?CSS_HEADER, Data, Req).

%% @doc Expand an incoming tag into the latest matching tag
%% Supported formats are:
%% *     - Latest version
%% X     - Latest minor in given major
%% X.Y   - Latest patch in given major and minor
%% X.Y.Z - Given version if it is in the given Tags
%%
%% all cases will throw an error if no version can be found
expand_tag(Tag0, Tags) ->
    case binary:split(Tag0, <<$.>>, [global]) of
        %% Exact version specified, check if it exists
        [_Major, _Minor, _Patch] = Version ->
            _Version = to_version(Version),
            true = lists:member(Tag0, Tags),
            Tag0;
        %% Expand to the latest version
        [<<"*">>] ->
            find_latest_version(fun compare_all/2, Tags);
        %% Expand to the latest minor and patch in a given major
        [Major0] ->
            Major1 = binary_to_integer(Major0),
            find_latest_version(compare_major(Major1), Tags);
        %% Expand to the latest tag in a given major and minor
        [Major0, Minor0] ->
            {Major1, Minor1} = {binary_to_integer(Major0), binary_to_integer(Minor0)},
            find_latest_version(compare_minor(Major1, Minor1), Tags)
    end.

%% @doc Compare all version
compare_all(Version0, Version1) ->
    case Version1 > Version0 of
        true  -> Version1;
        false -> Version0
    end.

%% @doc Compare only a given major version
compare_major(Major) ->
    %% Same major version, compare them
    fun ({_Major0, _Minor0, _Patch0} = Version0, {Major1, _Minor1, _Patch1} = Version1)
          when Major1 == Major ->
        case Version1 > Version0 of
            true  -> Version1;
            false -> Version0
        end;
    %% Different major, keep the acc
        (Version0, _Version1) ->
            Version0
    end.

%% @doc Compare a given major and minor version
compare_minor(Major, Minor) ->
    %% Same major version, compare them
    fun ({_Major0, _Minor0, _Patch0} = Version0, {Major1, Minor1, _Patch1} = Version1)
          when Major1 == Major, Minor1 == Minor ->
        case Version1 > Version0 of
            true  -> Version1;
            false -> Version0
        end;
    %% Different major, keep the acc
        (Version0, _Version1) ->
            Version0
    end.

%% @doc Retrive the latest version according to the given comparsion function
find_latest_version(MaybeSwap, Tags) ->
    find_latest_version({-1, -1, -1}, MaybeSwap, Tags).

find_latest_version(Version, _MaybeSwap, []) ->
    version_to_binary(Version);
find_latest_version(VersionAcc, MaybeSwap, [Tag | Tags]) ->
    NewVersion = try to_version(Tag) of
        %% Compare with the version we got
        Version -> MaybeSwap(VersionAcc, Version)
    catch
        %% Incorrect semver, ignore and move on
        error:_ -> VersionAcc
    end,
    find_latest_version(NewVersion, MaybeSwap, Tags).

%% @doc Version to binary
%% May throw error:badarg.
version_to_binary({Major, Minor, Patch}) when Major >= 0, Minor >= 0, Patch >= 0 ->
    <<(integer_to_binary(Major))/binary, $.,
      (integer_to_binary(Minor))/binary, $.,
      (integer_to_binary(Patch))/binary>>.

%% @doc Binary or list to version tuple
to_version(Version) when is_list(Version) ->
    case lists:map(fun binary_to_integer/1, Version) of
        [Major, Minor, Patch] when Major >= 0, Minor >= 0, Patch >= 0 ->
            {Major, Minor, Patch};
        _ ->
            error(badarg)
    end;
to_version(Tag) when is_binary(Tag) ->
    to_version(binary:split(Tag, <<$.>>, [global])).

terminate(_Reason, _Req, _State) ->
    ok.

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
