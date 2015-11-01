-module(ds_api_version_handler).

-export([init/3]).
-export([rest_init/2]).
-export([allowed_methods/2]).
-export([resource_exists/2]).
-export([is_authorized/2]).
-export([is_conflict/2]).
-export([generate_etag/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([delete_resource/2]).
-export([send_file/2]).
-export([to_text/2]).
-export([to_css/2]).
-export([add_version/2]).

-include_lib("kernel/include/file.hrl").

-record(state, {
          component,
          version,
          argument,
          data
         }).


init(_Type, Req, []) ->
    {upgrade, protocol, cowboy_rest, Req, no_state}.

rest_init(Req0, no_state) ->
    {Component, Req1} = cowboy_req:binding(component, Req0),
    {Version, Req2} = cowboy_req:binding(version, Req1),
    {Argument, Req3} = case cowboy_req:path_info(Req2) of
                           {[], Req} ->
                               {undefined, Req};
                           {[<<"css">>], Req} ->
                               {Variables0, ReqA} = cowboy_req:qs_vals(Req),
                               Variables1 = ds_api_css:variables_to_binary(Variables0),
                               Hash = ds_api_util:hash(Variables1),
                               {{css, Hash, Variables1}, ReqA};
                           {[<<"files">> | _] = Segments, Req} ->
                               {segments_to_path(Segments), Req};
                           {[<<"script.min.js">>], Req} ->
                               {<<"script.min.js">>, Req}
                   end,
    State = #state{component = Component, version = Version, argument = Argument},
    {ok, Req3, State}.

allowed_methods(Req, #state{argument = undefined} = State) ->
    {[<<"PUT">>, <<"DELETE">>, <<"OPTIONS">>], Req, State};
allowed_methods(Req, State) ->
    {[<<"GET">>, <<"HEAD">>, <<"OPTIONS">>], Req, State}.

resource_exists(Req, #state{component = Component, version = Version, argument = Argument} = State) ->
    case ds_api_component_mgr:exists(Component, Version) of
        true ->
            case Argument of
                undefined ->
                    {true, Req, State};
                {css, _Hash, _Variables} ->
                    CSSFiles = ds_api_css:get_local_files(Component, Version),
                    {CSSFiles =/= [], Req, State#state{data = CSSFiles}};
                File0 when is_binary(File0) ->
                    VersionDir = ds_api_component_mgr:version_dir(Component, Version),
                    File1 = filename:join(VersionDir, File0),
                    {filelib:is_regular(File1), Req, State#state{data = File1}}
            end;
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

generate_etag(Req, #state{component = Component, version = Version0, argument = {css, Hash, _Variables}} = State) ->
    Version1 = ds_api_version:to_binary(Version0),
    ETag = <<"components/", Component/binary, "/versions/", Version1/binary, "/css/", Hash/binary>>,
    {{strong, ETag}, Req, State};
generate_etag(Req, #state{component = Component, version = Version0, argument = File} = State) ->
    Version1 = ds_api_version:to_binary(Version0),
    ETag = <<"components/", Component/binary, "/versions/", Version1/binary, "/files/", File/binary>>,
    {{strong, ETag}, Req, State}.

content_types_provided(Req, #state{argument = undefined} = State) ->
    {[{{<<"text">>, <<"plain">>, []}, to_text}], Req, State};
content_types_provided(Req, #state{argument = {css, _Hash, _Variables}} = State) ->
    {[{{<<"text">>, <<"css">>, []}, to_css}], Req, State};
content_types_provided(Req, #state{argument = File} = State) ->
    {[{cow_mimetypes:all(File), send_file}], Req, State}.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"x-www-form-urlencoded">>, []}, add_version}], Req, State}.

to_text(Req, State) ->
    {<<>>, Req, State}.

send_file(Req0, #state{data = File0} = State) ->
    {ok, AcceptEncoding, Req1} = cowboy_req:parse_header(<<"accept-encoding">>, Req0, []),
    {File1, Req2} = case proplists:is_defined(<<"gzip">>, AcceptEncoding) of
                        true ->
                            Req = cowboy_req:set_resp_header(<<"content-encoding">>, <<"gzip">>, Req1),
                            {<<File0/binary, ".gz">>, Req};
                        false ->
                            {File0, Req1}
                    end,
    {ok, #file_info{size = Size}} = file:read_file_info(File1),
    SendFile = fun (Socket, Transport) -> Transport:sendfile(Socket, File1) end,
    {{stream, Size, SendFile}, Req2, State}.

to_css(Req0, #state{
                component = Component,
                version = Version,
                argument = {css, Hash, Variables},
                data = CSSFiles0
               } = State) ->
    {ok, AcceptEncoding, Req1} = cowboy_req:parse_header(<<"accept-encoding">>, Req0, []),
    {ok, CSSFiles1} = ds_api_css:compile_component_sass(Component, Version, Hash, Variables, CSSFiles0),
    {CSSFiles2, Req2} = case proplists:is_defined(<<"gzip">>, AcceptEncoding) of
                            true ->
                                Req = cowboy_req:set_resp_header(<<"content-encoding">>, <<"gzip">>, Req1),
                                GzippedCSSFiles = [<<File/binary, ".gz">> || File <- CSSFiles1],
                                {GzippedCSSFiles, Req};
                            false ->
                                {CSSFiles1, Req1}
                        end,
    TotalSize = lists:foldl(
                  fun (File, SizeAcc) ->
                          {ok, #file_info{size = Size}} = file:read_file_info(File),
                          Size + SizeAcc
                  end,
                  0,
                  CSSFiles2
                 ),
    SendFiles = fun (Socket, Transport) ->
                        lists:foreach(
                          fun (File) -> Transport:sendfile(Socket, File) end,
                          CSSFiles2
                         )
                end,
    {{stream, TotalSize, SendFiles}, Req2, State}.

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

segments_to_path([]) ->
    <<>>;
segments_to_path(PathList) ->
    <<$/, Path/binary>> =  segments_to_path(PathList, <<>>),
    Path.

segments_to_path([], PathAcc) ->
    PathAcc;
segments_to_path([Segment | Rest], PathAcc) ->
    segments_to_path(Rest, <<PathAcc/binary, $/, Segment/binary>>).

