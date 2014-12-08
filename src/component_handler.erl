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
            _          -> cowboy_req:reply(404, ?ACCESS_CONTROL_HEADER, Req2)
        end,
    {ok, Req3, State}.

handle_get(Req, _State=#state{}) ->
    {ComponentName, Req1} = cowboy_req:binding(component, Req),
    case component_exists(ComponentName) of
        true ->
            get_component_data(Req1, ComponentName);
        false ->
            cowboy_req:reply(404, ?ACCESS_CONTROL_HEADER, Req1)
    end.

component_exists(ComponentName) ->
    {ok, RepoDir} = application:get_env(divapi, repo_dir),
    filelib:is_dir(RepoDir ++ "/" ++ binary_to_list(ComponentName) ++ ".git").

get_component_data(Req, ComponentName) ->
    {PathInfo, Req1} = cowboy_req:path_info(Req),
    Tags = git_utils:tags(ComponentName),
    {ok, Req2} =
        case PathInfo of
            [] ->
                cowboy_req:reply(200, ?JSON_HEADER ++ ?ACCESS_CONTROL_HEADER,
                                 jiffy:encode(Tags), Req1);
            [PartialTag | Routes] ->
                Tag = expand_tag(PartialTag, Tags),
                case Routes of
                    [] ->
                        case git_utils:get_diversity_json(ComponentName, Tag) of
                            undefined ->
                                cowboy_req:reply(404, ?ACCESS_CONTROL_HEADER, Req1);
                            Data      ->
                                cowboy_req:reply(200, ?JSON_HEADER ++ ?ACCESS_CONTROL_HEADER,
                                                 Data, Req1)
                        end;
                    [Settings] when Settings =:= <<"settings">>;
                                    Settings =:= <<"settingsForm">> ->
                        case git_utils:get_diversity_json(ComponentName, Tag) of
                            undefined ->
                                cowboy_req:reply(404, ?ACCESS_CONTROL_HEADER, Req1);
                            Json ->
                                {DiversityData} = jiffy:decode(Json),
                                SettingsBinary = proplists:get_value(Settings, DiversityData, {[]}),
                                SettingsJson = jiffy:encode(SettingsBinary),
                                cowboy_req:reply(200, ?JSON_HEADER ++ ?ACCESS_CONTROL_HEADER,
                                                 SettingsJson, Req1)
                        end;
                    [<<"files">> | Path] ->
                        File = filename:join(Path),
                        case cowboy_req:header(<<"if-none-match">>, Req1) of
                            {File, Req} ->
                                cowboy_req:reply(304, ?ACCESS_CONTROL_HEADER, Req1);
                            _    ->
                                FileBin = git_utils:get_file(ComponentName, Tag, File),
                                case FileBin of
                                    undefined ->
                                        cowboy_req:reply(404, ?ACCESS_CONTROL_HEADER, Req1);
                                    error ->
                                        cowboy_req:reply(500, ?ACCESS_CONTROL_HEADER, Req1);
                                    _ ->
                                        {Mime, Type, []} = cow_mimetypes:all(File),
                                        Headers = [{<<"content-type">>,
                                                    << Mime/binary, "/", Type/binary >>}],
                                        Headers1 = case Tag of
                                            <<"HEAD">> ->
                                                Headers;
                                            _ ->
                                                Headers ++
                                                [{<<"Cache-Control">>,
                                                  <<"max-age=90000">>},
                                                 {<<"Etag">>, File}]
                                        end,
                                        cowboy_req:reply(200, Headers1 ++ ?ACCESS_CONTROL_HEADER,
                                                         FileBin, Req1)
                                end
                            end;
                    _ ->
                        cowboy_req:reply(404, Req1)
                end;
            _ ->
                cowboy_req:reply(404, Req1)
        end,
    {ok, Req2}.

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
