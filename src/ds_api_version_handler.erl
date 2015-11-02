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
-export([to_json/2]).
-export([to_css/2]).
-export([handle_add_version/2]).

-include_lib("kernel/include/file.hrl").

-type argument() :: {css, binary(), binary(), [binary()]} | binary() | undefined.

-record(state, {
          component :: binary(),
          version   :: ds_api_version:version(),
          argument  :: argument()
         }).


init(_Type, Req, []) ->
    {upgrade, protocol, cowboy_rest, Req, no_state}.

rest_init(Req0, no_state) ->
    {Component, Req1} = cowboy_req:binding(component, Req0),
    {Version, Req2} = cowboy_req:binding(version, Req1),
    {Argument, Req3} = get_argument(Component, Version, Req2),
    {ok, Req3, #state{component = Component, version = Version, argument = Argument}}.

get_argument(Component, Version, Req0) ->
    {PathInfo, Req1} = cowboy_req:path_info(Req0),
    case PathInfo of
        [] ->
            {undefined, Req1};
        [<<"css">>] ->
            {Variables0, Req2} = cowboy_req:qs_vals(Req1),
            Variables1 = ds_api_css:variables_to_binary(Variables0),
            Hash = ds_api_util:hash(Variables1),
            CSSFiles = ds_api_css:style_files(Component, Version),
            {{css, Hash, Variables1, CSSFiles}, Req2};
        Segments ->
            VersionDir = ds_api_component:version_dir(Component, Version),
            {filename:join(VersionDir, segments_to_path(Segments)), Req1}
    end.

allowed_methods(Req, #state{argument = undefined} = State) ->
    {[<<"PUT">>, <<"DELETE">>, <<"OPTIONS">>], Req, State};
allowed_methods(Req, State) ->
    {[<<"GET">>, <<"HEAD">>, <<"OPTIONS">>], Req, State}.

resource_exists(Req, #state{component = Component, version = Version, argument = Argument} = State) ->
    Exists = case Argument of
                 undefined ->
                     filelib:is_dir(ds_api_component:version_dir(Component, Version));
                 {css, _, _, CSSFiles} ->
                     CSSFiles =/= undefined andalso CSSFiles =/= [];
                 File when is_binary(File) ->
                     filelib:is_regular(File)
             end,
    {Exists, Req, State}.

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
    {filelib:is_dir(ds_api_component:version_dir(Component, Version)), Req, State}.

generate_etag(Req, #state{component = Component, version = Version0, argument = {css, Hash, _, _}} = State) ->
    Version1 = ds_api_version:to_binary(Version0),
    ETag = <<"components/", Component/binary, "/versions/", Version1/binary, "/css/", Hash/binary>>,
    {{strong, ETag}, Req, State};
generate_etag(Req, #state{component = Component, version = Version0, argument = File} = State) when is_binary(File) ->
    Version1 = ds_api_version:to_binary(Version0),
    ETag = <<"components/", Component/binary, "/versions/", Version1/binary, File/binary>>,
    {{strong, ETag}, Req, State};
generate_etag(Req, State) ->
    {false, Req, State}.

content_types_provided(Req, #state{argument = undefined} = State) ->
    {[{{<<"application">>, <<"json">>, []}, to_json}], Req, State};
content_types_provided(Req, #state{argument = {css, _, _, _}} = State) ->
    {[{{<<"text">>, <<"css">>, []}, to_css}], Req, State};
content_types_provided(Req, #state{argument = File} = State) when is_binary(File) ->
    {[{cow_mimetypes:all(File), send_file}], Req, State}.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"x-www-form-urlencoded">>, []}, handle_add_version}], Req, State}.

to_json(Req, State) ->
    {jiffy:encode(#{}), Req, State}.

send_file(Req0, #state{argument = File0} = State) ->
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
                version   = Version,
                argument  = {css, Hash, Variables, CSSFiles0}
               } = State) ->
    case ds_api_css:compile_component_sass(Component, Version, Hash, Variables, CSSFiles0) of
        {ok, CSSFiles1} ->
            send_css_files(CSSFiles1, Req0, State);
        {error, Error} ->
            {false, cowboy_req:set_resp_body(Error, Req0), State}
    end.

send_css_files(CSSFiles0, Req0, State) ->
    {ok, AcceptEncoding, Req1} = cowboy_req:parse_header(<<"accept-encoding">>, Req0, []),
    {CSSFiles1, Req2} = case proplists:is_defined(<<"gzip">>, AcceptEncoding) of
                            true ->
                                Req = cowboy_req:set_resp_header(<<"content-encoding">>, <<"gzip">>, Req1),
                                GzippedCSSFiles = [<<File/binary, ".gz">> || File <- CSSFiles0],
                                {GzippedCSSFiles, Req};
                            false ->
                                {CSSFiles0, Req1}
                        end,
    TotalSize = lists:foldl(
                  fun (File, SizeAcc) ->
                          {ok, #file_info{size = Size}} = file:read_file_info(File),
                          Size + SizeAcc
                  end,
                  0,
                  CSSFiles1
                 ),
    SendFiles = fun (Socket, Transport) ->
                        lists:foreach(
                          fun (File) -> Transport:sendfile(Socket, File) end,
                          CSSFiles1
                         )
                end,
    {{stream, TotalSize, SendFiles}, Req2, State}.

handle_add_version(Req0, #state{component = Component, version = Version} = State) ->
    case ds_api_component:add_version(Component, Version) of
        ok ->
            {true, Req0, State};
        {error, Error} ->
            Req1 = cowboy_req:set_resp_body(Error, Req0),
            {false, Req1, State}
    end.

delete_resource(Req, #state{component = Component, version = Version} = State) ->
    ok = ds_api_component:delete_version(Component, Version),
    {true, Req, State}.

segments_to_path([]) ->
    <<>>;
segments_to_path(PathList) ->
    <<$/, Path/binary>> =  segments_to_path(PathList, <<>>),
    Path.

segments_to_path([], PathAcc) ->
    PathAcc;
segments_to_path([Segment | Rest], PathAcc) ->
    segments_to_path(Rest, <<PathAcc/binary, $/, Segment/binary>>).
