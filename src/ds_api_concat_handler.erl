-module(ds_api_concat_handler).

-export([init/2]).
-export([allowed_methods/2]).
-export([resource_exists/2]).
-export([generate_etag/2]).
-export([last_modified/2]).
-export([content_types_provided/2]).
-export([send_files/2]).

-record(state, {
          type                              :: css | js,
          files    = []                     :: list(binary() | pid()),
          size     = 0                      :: non_neg_integer(),
          hash                              :: binary(),
          modified = {{0, 0, 0}, {0, 0, 0}} :: calendar:datetime(),
          expanded = false                  :: boolean(),
          gzip     = false                  :: boolean(),
          stage                             :: binary() | undefined,
          tmp_dir                           :: binary() | undefined
         }).

init(Req0, Type) ->
    Req1 = ds_api_util:set_access_control_headers(Req0),
    {cowboy_rest, Req1, init_state(Req1, Type)}.

init_state(Req, Type) ->
    ComponentVersions = cowboy_req:parse_qs(Req),
    {Stage, TmpDir} = case cowboy_req:binding(stage, Req) of
                          undefined ->
                              {undefined, undefined};
                          Stage0 ->
                              TmpPath = ds_api_util:tmp_path(),
                              ok = file:make_dir(TmpPath),
                              {Stage0, TmpPath}
                      end,
    State = #state{
               type    = Type,
               hash    = ds_api_util:hash(cowboy_req:qs(Req)),
               gzip    = ds_api_util:should_gzip(Req),
               stage   = Stage,
               tmp_dir = TmpDir
              },
    do_init_state(ComponentVersions, State).

do_init_state([], #state{files = Files} = State) ->
    State#state{files = lists:reverse(Files)};
do_init_state([{Component, BinVersion} | ComponentVersions], State0) ->
    #state{
       type     = Type,
       files    = Files,
       size     = TotalSize,
       modified = Modified0,
       expanded = Expanded0,
       gzip     = GZip,
       stage    = Stage,
       tmp_dir  = TmpDir
      } = State0,
    case ds_api_version:to_exact_version(Component, BinVersion) of
        {ok, Version, Expanded} ->
            case get_file_info(Type, Component, Version, GZip, Stage, TmpDir) of
                {_File, {undefined, undefined}} ->
                    State0#state{files = []};
                {File, {Size, Modified}} ->
                    Modified1 = case Modified > Modified0 of
                                    true  -> Modified;
                                    false -> Modified0
                                end,
                    State1 = State0#state{
                               files    = [File | Files],
                               size     = TotalSize + Size,
                               modified = Modified1,
                               expanded = Expanded0 orelse Expanded
                              },
                    do_init_state(ComponentVersions, State1)
            end;
        {error, badarg} ->
            State0#state{files = []}
    end.

get_file_info(Type, Component, Version, GZip, undefined, undefined) ->
    VersionDir = ds_api_component:version_dir(Component, Version),
    get_file_info(Type, VersionDir, GZip);
get_file_info(Type, Component, Version, GZip, Stage, TmpDir) ->
    case ds_api_stage:init_version_dir(Component, Stage, TmpDir) of
        undefined  -> get_remote_file_info(Type, Component, Version, GZip);
        VersionDir -> get_file_info(Type, VersionDir, GZip)
    end.

get_file_info(js, VersionDir, GZip) ->
    File0 = filename:join(VersionDir, <<"script.min.js">>),
    File1 = ds_api_util:maybe_gzipped(File0, GZip),
    {File1, ds_api_util:get_file_info(File1)};
get_file_info(css, VersionDir, GZip) ->
    case ds_api_css:compile_and_concatenate(VersionDir) of
        {ok, CSSFile0} ->
            CSSFile1 = ds_api_util:maybe_gzipped(CSSFile0, GZip),
            {CSSFile1, ds_api_util:get_file_info(CSSFile1)};
        _Error ->
            {undefined, {undefined, undefined}}
    end.

get_remote_file_info(Type, Component, Version, GZip) ->
    Endpoint = case Type of
                   js  -> <<"script.min.js">>;
                   css -> <<"css">>
               end,
    case ds_api_stage:stream_remote_file(Component, Version, GZip, Endpoint) of
        {ok, RequestId} -> {RequestId, ds_api_stage:get_remote_file_info(RequestId)};
        _Error          -> {undefined, {undefined, undefined}}
    end.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"HEAD">>, <<"OPTIONS">>], Req, State}.

resource_exists(Req, #state{files = Files} = State) ->
    {Files =/= [], Req, State}.

generate_etag(Req, #state{type = Type, hash = Hash, expanded = false} = State) ->
    {{strong, <<(atom_to_binary(Type, utf8))/binary, $/, Hash/binary>>}, Req, State};
generate_etag(Req, State) ->
    {undefined, Req, State}.

last_modified(Req, #state{modified = Modified} = State) ->
    {Modified, Req, State}.

content_types_provided(Req, #state{type = Type} = State) ->
    ContentType = case Type of
                      js  -> {<<"application">>, <<"javascript">>, []};
                      css -> {<<"text">>, <<"css">>, []}
                  end,
    {[{ContentType, send_files}], Req, State}.

send_files(Req, #state{files = Files, size = Size, tmp_dir = TmpDir} = State) ->
    SendFiles = fun (Socket, Transport) ->
                        lists:foreach(
                          fun (File) when is_binary(File) ->
                                  Transport:sendfile(Socket, File);
                              (RequestId) when is_reference(RequestId) ->
                                  ds_api_stage:send_stream(Transport, Socket, RequestId)
                          end,
                          Files
                         ),
                        ds_api_util:delete_dir(TmpDir)
                end,
    {{stream, Size, SendFiles}, Req, State}.
