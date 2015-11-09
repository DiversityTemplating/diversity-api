-module(ds_api_version_handler).

-export([init/2]).
-export([allowed_methods/2]).
-export([malformed_request/2]).
-export([resource_exists/2]).
-export([previously_existed/2]).
-export([moved_temporarily/2]).
-export([is_authorized/2]).
-export([is_conflict/2]).
-export([generate_etag/2]).
-export([last_modified/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([delete_resource/2]).
-export([send_file/2]).
-export([errors_to_json/2]).
-export([handle_add_version/2]).

-include_lib("kernel/include/file.hrl").

-record(state, {
          action      :: modify | file | css,
          component   :: binary(),
          version     :: ds_api_version:version() | undefined,
          version_dir :: binary() | undefined,
          expanded    :: boolean(),
          file        :: binary() | undefined,
          size        :: non_neg_integer() | undefined,
          modified    :: calendar:datetime(),
          gzip        :: boolean(),
          stage       :: binary() | undefined
         }).

init(Req0, []) ->
    Req1 = ds_api_util:set_access_control_headers(Req0),
    {cowboy_rest, Req1, init_state(Req1)}.

init_state(Req) ->
    Component = cowboy_req:binding(component, Req),
    Version0 = cowboy_req:binding(version, Req),
    Stage = cowboy_req:binding(stage, Req),
    {Version1, Expanded} = ds_api_version:expand(Component, Version0),
    VersionDir = case is_binary(Stage) of
                     true  -> ds_api_stage:init_version_dir(Component, Stage);
                     false -> ds_api_component:version_dir(Component, Version1)
                 end,
    State = #state{
               component   = Component,
               version     = Version1,
               version_dir = VersionDir,
               expanded    = Expanded,
               gzip        = ds_api_util:should_gzip(Req),
               stage       = Stage
              },
    init_state(State, Req).

init_state(#state{version_dir = VersionDir, gzip = GZip} = State, Req) ->
    case cowboy_req:path_info(Req)of
        %% Add or delete the version
        [] ->
            State#state{action = modify};
        PathInfo ->
            File0 = case PathInfo of
                        %% Retrive compiled and concatenated CSS, if the resource hasn't been
                        %% compiled beforehand then compile it here and return the resulting file
                        [<<"css">>] ->
                            Variables = cowboy_req:parse_qs(Req),
                            case ds_api_css:compile_and_concatenate(VersionDir, Variables) of
                                {ok, CSSFile} -> CSSFile;
                                _Error        -> undefined
                            end;
                        %% Otherwise a file has been asked for
                        Segments ->
                            Path = segments_to_path(Segments),
                            filename:join(VersionDir, Path)
                    end,
            File1 = ds_api_util:maybe_gzipped(File0, GZip),
            {Size, Modified} = ds_api_util:get_file_info(File1),
            State#state{action = file, file = File0, size = Size, modified = Modified}
    end.

%% It's only possible to add/delete versions in production, not in stage mode
allowed_methods(Req, #state{action = modify, stage = undefined} = State) ->
    {[<<"PUT">>, <<"DELETE">>, <<"OPTIONS">>], Req, State};
allowed_methods(Req, State) ->
    {[<<"GET">>, <<"HEAD">>, <<"OPTIONS">>], Req, State}.

%% It's only allowed to add/delete if we have a version
malformed_request(Req, #state{action = modify, version = Version} = State) ->
    {Version =:= undefined, Req, State};
malformed_request(Req, State) ->
    {false, Req, State}.

resource_exists(Req, #state{action = modify, version_dir = VersionDir} = State) ->
    {filelib:is_dir(VersionDir), Req, State};
resource_exists(Req, #state{size = undefined} = State) ->
    {false, Req, State};
resource_exists(Req, State) ->
    {true, Req, State}.

previously_existed(Req, #state{stage = undefined} = State) ->
    {false, Req, State};
previously_existed(Req, State) ->
    {true, Req, State}.

moved_temporarily(Req, #state{version_dir = undefined, stage = Stage} = State) 
  when is_binary(Stage) ->
    FallbackAPI = ds_api:fallback_api(),
    HostURL = cowboy_req:host_url(Req),
    HostURLSize = byte_size(HostURL),
    <<_:HostURLSize/binary, RestURL/binary>> = cowboy_req:url(Req),
    {{true, <<FallbackAPI/binary, RestURL/binary>>}, Req, State};
moved_temporarily(Req, State) ->
    {false, Req, State}.

is_authorized(Req, #state{action = modify} = State) ->
    {ds_api_auth:is_authorized(Req), Req, State};
is_authorized(Req, State) ->
    {true, Req, State}.

is_conflict(Req, #state{version_dir = VersionDir} = State) ->
    {filelib:is_dir(VersionDir), Req, State}.

generate_etag(Req, #state{file = File, expanded = false} = State) ->
    RootDir = ds_api:root_dir(),
    RootDirSize = byte_size(RootDir),
    Path = binary_part(File, RootDirSize, byte_size(File) - RootDirSize),
    {{strong, Path}, Req, State};
generate_etag(Req, State) ->
    {undefined, Req, State}.

last_modified(Req, #state{modified = Modified} = State) ->
    {Modified, Req, State}.

content_types_provided(Req, #state{action = modify} = State) ->
    {[{{<<"application">>, <<"json">>, []}, errors_to_json}], Req, State};
content_types_provided(Req, #state{file = File} = State) when is_binary(File) ->
    {[{cow_mimetypes:all(File), send_file}], Req, State}.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"x-www-form-urlencoded">>, []}, handle_add_version}], Req, State}.

%% TODO: Return errors from add/delete here
errors_to_json(Req, State) ->
    {jiffy:encode(#{}), Req, State}.

send_file(Req0, #state{file = File0, size = Size, gzip = GZip, version_dir = VersionDir, stage = Stage} = State) ->
    {File1, Req1} = maybe_compressed_file(File0, GZip, Req0),
    SendFile = fun (Socket, Transport) ->
                       Transport:sendfile(Socket, File1),
                       case is_binary(Stage) of
                           true  -> ds_api_util:delete_dir(VersionDir);
                           false -> ok
                       end
               end,
    {{stream, Size, SendFile}, Req1, State}.

maybe_compressed_file(File0, GZip, Req) ->
    case ds_api_util:maybe_gzipped(File0, GZip) of
        File0 -> {File0, Req};
        File1 -> {File1, cowboy_req:set_resp_header(<<"content-encoding">>, <<"gzip">>, Req)}
    end.

%% TODO: Return errors
handle_add_version(Req, #state{component = Component, version = Version} = State) ->
    Added = case do_add_version(Component, Version) of
                {ok, Result}    -> lists:all(fun (R) -> R =:= ok end, Result);
                {error, _Error} -> false
            end,
    case Added of
        true  -> ok;
        false -> do_delete_version(Component, Version)
    end,
    {Added, Req, State}.

delete_resource(Req, #state{component = Component, version = Version} = State) ->
    Deleted = case do_delete_version(Component, Version) of
                  {ok, _Result} -> true;
                  _Error        -> false
              end,
    {Deleted, Req, State}.

do_add_version(Component, Version) ->
    ds_api_util:multicall(ds_api_component, add_version, [Component, Version]).

do_delete_version(Component, Version) ->
    ds_api_util:multicall(ds_api_component, delete_version, [Component, Version]).

segments_to_path([]) ->
    <<>>;
segments_to_path(PathList) ->
    <<$/, Path/binary>> =  segments_to_path(PathList, <<>>),
    Path.

segments_to_path([], PathAcc) ->
    PathAcc;
segments_to_path([Segment | Rest], PathAcc) ->
    segments_to_path(Rest, <<PathAcc/binary, $/, Segment/binary>>).
