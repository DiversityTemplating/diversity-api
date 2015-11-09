-module(ds_api_auth).

-export([is_authorized/1]).

%% @doc Helper function for determining if a request should be authorized. This is used for all
%% request that modifies some resource. We only support basic auth. The config parameter
%% api_keys should be a map with {User, Password}-pairs.
%% If api_keys is not defined then authorization is turned off.
-spec is_authorized(cowboy_req:req()) -> boolean().
is_authorized(Req) ->
    case ds_api:api_keys() of
        APIKeys when is_map(APIKeys) ->
            case cowboy_req:parse_header(<<"authorization">>, Req) of
                {basic, User, Password} ->
                    case maps:get(User, APIKeys, undefined) =:= Password of
                        true  -> true;
                        false -> {false, <<"basic">>}
                    end;
                _Otherwise ->
                    {false, <<"basic">>}
            end;
        _Otherwise ->
            true
    end.
