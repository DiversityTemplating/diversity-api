-module(component_handler).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {
}).

-define(JSON_HEADER, [{<<"content-type">>, <<"application/json">>}]).

init(_, Req, _Opts) ->
	{ok, Req, #state{}}.

handle(Req, State=#state{}) ->
    {PathInfo, Req2} = cowboy_req:path_info(Req),
    {ComponentName, Req3} = cowboy_req:binding(component, Req2),
    Tags = git_utils:tags(ComponentName),
    {ok, Req4} =
        case PathInfo of
            [] ->
                cowboy_req:reply(200, ?JSON_HEADER, jiffy:encode(Tags), Req3);
            [<<"*">>] ->
                Tag = find_latest_tag(Tags),
                Data = git_utils:get_diversity_json(ComponentName, Tag),
                cowboy_req:reply(200, ?JSON_HEADER, Data, Req3);
            [Tag] when is_binary(Tag) ->
                %%TagString = binary_to_list(Tag),
                Data = case lists:member(Tag, Tags) of
                    true ->
                        git_utils:get_diversity_json(ComponentName, Tag);
                    false ->
                        PatchNr = find_latest_patch(Tag, Tags),
                        TagWithPatch = <<Tag/binary, ".", PatchNr/binary>>,
                        %% Ensure that it's an valid tag
                        true = lists:member(TagWithPatch, Tags),
                        git_utils:get_diversity_json(ComponentName, TagWithPatch)
                end,
                cowboy_req:reply(200, ?JSON_HEADER, Data, Req3);
            _ ->
                cowboy_req:reply(404, Req3)
        end,
    {ok, Req4, State}.

find_latest_tag([]) ->
    <<"">>;
find_latest_tag(Tags) ->
    NumericTags = lists:filter(fun(X) ->
        case re:run(binary_to_list(X), "^\\d\\.\\d\\.\\d$") of
            nomatch -> false; _ -> true
        end
    end, Tags),
    lists:last(lists:sort(NumericTags)).

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
