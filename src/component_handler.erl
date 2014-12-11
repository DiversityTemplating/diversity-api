-module(component_handler).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {}).

-define(JSON_HEADER, [{<<"content-type">>, <<"application/json">>}]).
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

handle_get(Req, _State=#state{}) ->
    {ComponentName, Req1} = cowboy_req:binding(component, Req),
    case component_exists(ComponentName) of
        true ->
            get_component_data(Req1, ComponentName);
        false ->
            reply_from_result_data(Req1, resource_not_found)
    end.

component_exists(ComponentName) ->
    {ok, RepoDir} = application:get_env(divapi, repo_dir),
    filelib:is_dir(RepoDir ++ "/" ++ binary_to_list(ComponentName) ++ ".git").

get_component_data(Req, ComponentName) ->
    {PathInfo, Req1} = cowboy_req:path_info(Req),
    ResultData = case PathInfo of
        undefined ->
            serve_tags(ComponentName);
        [PartialTag | Routes] ->
            Tag = expand_tag(PartialTag, get_tags(ComponentName)),
            case Routes of
                [] ->
                    serve_diversity_json(ComponentName, Tag);
                [Settings] when Settings =:= <<"settings">>;
                                Settings =:= <<"settingsForm">> ->
                    serve_setting_json(ComponentName, Tag, Settings);
                [<<"files">> | Path] ->
                    File = filename:join(Path),
                    {BrowserCacheKey, _} = cowboy_req:header(<<"if-none-match">>, Req1),
                    serve_file(ComponentName, Tag, File, BrowserCacheKey);
                _ ->
                    resource_not_found
            end;
        _ ->
            resource_not_found
    end,
    reply_from_result_data(Req1, ResultData).

reply_from_result_data(Req, file_not_changed) ->
    cowboy_req:reply(304, ?ACCESS_CONTROL_HEADER, Req);
reply_from_result_data(Req, resource_not_found) ->
    cowboy_req:reply(404, Req);
reply_from_result_data(Req, {error, Reason}) ->
    cowboy_req:reply(500, ?ACCESS_CONTROL_HEADER, Reason, Req);
reply_from_result_data(Req, {ok, {Headers, Data}}) ->
    cowboy_req:reply(200, ?ACCESS_CONTROL_HEADER ++ Headers, Data, Req);
reply_from_result_data(Req, {ok, Data}) ->
    cowboy_req:reply(200, ?ACCESS_CONTROL_HEADER ++ ?JSON_HEADER, Data, Req).

get_tags(ComponentName) ->
    git_utils:tags(ComponentName).

serve_tags(ComponentName) ->
    {ok, jiffy:encode(get_tags(ComponentName))}.

serve_diversity_json(ComponentName, Tag) ->
    case git_utils:get_diversity_json(ComponentName, Tag) of
        undefined -> resource_not_found;
        Data      -> {ok, Data}
    end.

serve_setting_json(ComponentName, Tag, Settings) ->
    case git_utils:get_diversity_json(ComponentName, Tag) of
        undefined ->
            resource_not_found;
        Json ->
            {DiversityData} = jiffy:decode(Json),
            SettingsBinary = proplists:get_value(Settings, DiversityData, {[]}),
            SettingsJson = jiffy:encode(SettingsBinary),
            {ok, SettingsJson}
    end.

serve_file(_ComponentName, Tag, File, BrowserCacheKey) when <<Tag/binary, File/binary>> ==
                                                            BrowserCacheKey ->
    file_not_changed;
serve_file(ComponentName, Tag, File, BrowserCacheKey) ->
    io:format("~p == ~p", [File, BrowserCacheKey]),
    case  git_utils:get_file(ComponentName, Tag, File) of
        undefined ->
            resource_not_found;
        error ->
            %% Should get a reason from git_utils get_file command.
            {error, <<"Error while fetching file">>};
        FileBin ->
            {Mime, Type, []} = cow_mimetypes:all(File),
            Headers = [{<<"content-type">>,
                        << Mime/binary, "/", Type/binary >>}],
            Headers1 = case Tag of
                <<"HEAD">> -> Headers;
                _          -> Headers ++ [{<<"Cache-Control">>, <<"max-age=90000">>},
                                          {<<"Etag">>, <<Tag/binary, File/binary>>}]
            end,
            {ok, {Headers1, FileBin}}
    end.


expand_tag(<<"*">>, _Tags) -> <<"HEAD">>;
expand_tag(Tag, Tags) ->
    case lists:member(Tag, Tags) of
        true ->
            Tag;
        false ->
            PatchNr = find_latest_patch(Tag, Tags),
            <<Tag/binary, ".", PatchNr/binary>>
    end.

find_latest_patch(PrefixTag, Tags) ->
    lists:foldl(fun(Tag, LatestPatch) ->
        case binary:longest_common_prefix([Tag, PrefixTag])of
            3 ->
                %% <<"1.2.3">>, the patch nr is char 4
                PatchNr = binary:part(Tag, 4, 1),
                case LatestPatch =< PatchNr of
                    true -> PatchNr;
                    false -> LatestPatch
                end;
            _ -> LatestPatch
        end
    end, <<"0">>, Tags).

terminate(_Reason, _Req, _State) ->
    ok.
