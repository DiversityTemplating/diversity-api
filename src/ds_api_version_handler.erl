-module(ds_api_version_handler).

-export([init/3]).
-export([allowed_methods/2]).
-export([resource_exists/2]).
-export([is_authorized/2]).
-export([is_conflict/2]).
-export([generate_etag/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([delete_resource/2]).
-export([send_file/2]).
-export([add_version/2]).

-record(state, {
          component,
          version,
          argument,
          data
         }).


init(_Type, Req0, []) ->
    {Component, Req1} = cowboy_req:binding(component, Req0),
    {Version, Req2} = cowboy_req:binding(version, Req1),
    {Argument, Req3} = case cowboy_req:pathinfo(Req2) of
                       {[], Req} ->
                           {undefined, Req};
                       {[<<"files">> | Segments], Req} ->
                           {segments_to_path(Segments), Req}
                   end,
    State = #state{component = Component, version = Version, argument = Argument},
    {upgrade, protocol, cowboy_rest, Req3, State}.

allowed_methods(Req, #state{argument = undefined} = State) ->
    {[<<"PUT">>, <<"DELETE">>, <<"OPTIONS">>], Req, State};
allowed_methods(Req, State) ->
    {[<<"GET">>, <<"HEAD">>, <<"OPTIONS">>], Req, State}.

resource_exists(Req, #state{component = Component, version = Version, argument = undefined} = State) ->
    {ds_api_component_mgr:exists(Component, Version), Req, State};
resource_exists(Req, #state{component = Component, version = Version, argument = File0} = State) ->
    case ds_api_component_mgr:exists(Component, Version) of
        true ->
            File1 = ds_api_component_mgr:file_path(Component, Version, File0),
            {filelib:is_regular(File1), Req, State#state{data = File1}};
        false ->
            {false, Req, State}
    end.

is_authorized(Req0, State) ->
    case cowboy_req:method(Req0) of
        {Method, Req1} when Method =:= <<"PUT">>;
                            Method =:= <<"DELETE">> ->
            % TODO: API-KEY
            {true, Req1, State};
        _ ->
            {true, Req0, State}
    end.

is_conflict(Req, #state{component = Component, version = Version} = State) ->
    {ds_api_component_mgr:exists(Component, Version), Req, State}.

generate_etag(Req, {Component, Version0, File}) ->
    Version1 = ds_api_version:to_binary(Version0),
    ETag = <<"components/", Component/binary, "/versions/", Version1/binary, "/files/", File/binary>>,
    {{strong, ETag}, Req, Component}.

content_types_provided(Req, {_Component, _Version, File} = State) ->
    {[{cow_mimetypes:all(File), send_file}], Req, State}.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"x-www-form-urlencoded">>, []}, add_version}], Req, State}.

send_file(Req, #state{data = File} = State) ->
    SendFile = fun (Transport, Socket) -> Transport:sendfile(Socket, File) end,
    {SendFile, Req, State}.

add_version(Req, #state{component = Component, version = Version} = State) ->
    case ds_api_component_mgr:add_version(Component, Version) of
        ok ->
            ok = ds_api_component_mgr:publish(Component, Version),
            {true, Req, State};
        {error, _Error} ->
            {false, Req, State}
    end.

delete_resource(Req, #state{component = Component, version = Version} = State) ->
    case ds_api_component_mgr:delete_version(Component, Version) of
        ok              -> {true, Req, State};
        {error, _Error} -> {false, Req, State}
    end.

segments_to_path(PathList) ->
    segments_to_path(PathList, <<>>).

segments_to_path([], PathAcc) ->
    PathAcc;
segments_to_path([Segment | Rest], PathAcc) ->
    segments_to_path(Rest, <<PathAcc/binary, $/, Segment/binary>>).

