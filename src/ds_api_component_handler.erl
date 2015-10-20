-module(ds_api_component_handler).

-export([init/3]).
-export([rest_init/2]).
-export([allowed_methods/2]).
-export([resource_exists/2]).
-export([content_types_provided/2]).
-export([serve_css/2]).
-export([serve_json/2]).
-export([serve_file/2]).
-export([generate_etag/2]).

-record(state, {
          action,
          component,
          tag,
          data
         }).

init(_Type, Req0, [Action]) ->
    {Component, Req1} = cowboy_req:binding(component, Req0),
    {Tag0, Req2} = cowboy_req:binding(tag, Req1),
    Tag1 = ds_api_component:expand_tag(Component, Tag0),
    State = #state{action = Action, component = Component, tag = Tag1},
    {upgrade, protocol, cowboy_rest, Req2, State}.

rest_init(Req, #state{tag = undefined} = State) ->
    {ok, Req, State};
rest_init(Req0, State) ->
    #state{component = Component, tag = Tag, action = Action} = State,
    {Data, Req1} = case Action of
                       diversityJSON ->
                           {<<"files/diversity.json">>, Req0};
                       javascript ->
                           {<<"script.min.js">>, Req0};
                       css ->
                           Diversity = ds_api_component:diversityJSON(Component, Tag),
                           {Variables0, Req} = cowboy_req:qs_vals(Req0),
                           ToBinary = fun ({Variable, Value}, Acc) ->
                                              BinVar = <<$$, Variable/binary, $:, Value/binary, $;>>,
                                              <<Acc/binary, BinVar/binary>>
                                      end,
                           Variables1 = lists:foldl(ToBinary, <<>>, Variables0),

                           Hash = crypto:hash(sha1, Variables1),
                           CSSStyles = case maps:get(<<"styles">>, Diversity) of
                                           Style when is_binary(Style) -> [Style];
                                           Styles when is_list(Styles) -> Styles;
                                           undefined                   -> undefined
                                       end,
                           Hash = case CSSStyles of
                                      undefined -> undefined;
                                      _         -> crypto:hash(sha1, Variables1)
                                  end,
                           CSSData = #{styles => CSSStyles,
                                       variables => Variables1,
                                       hash => Hash},
                           {CSSData, Req};
                       file ->
                           {File, Req} = cowboy_req:binding(file, Req0),
                           {filename:join(files, File), Req};
                       _ when Action =:= settings;
                              Action =:= settingsForm;
                              Action =:= thumbnail ->
                           {ds_api_component:Action(Component, Tag), Req0}
                   end,
    State = #state{
               action = Action,
               component = Component,
               data = Data
              },
    {ok, Req1, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"OPTIONS">>], Req, State}.

resource_exists(Req, #state{component = Component, tag = Tag, data = Path} = State)
  when is_binary(Path) ->
    AbsPath = filename:join(ds_api_component:path(Component, Tag), Path),
    {filelib:is_file(AbsPath), Req, State#state{data = AbsPath}};
resource_exists(Req, #state{action = css, data = #{styles := Styles}} = State) ->
    {Styles =/= undefined andalso Styles =/= [], Req, State};
resource_exists(Req, #state{data = JSON} = State) when is_map(JSON) ->
    {true, Req, State};
resource_exists(Req, #state{data = undefined} = State) ->
    {false, Req, State}.

content_types_provided(Req, #state{action = css} = State) ->
    {[{{<<"text">>, <<"css">>, []}, serve_css}], Req, State};
content_types_provided(Req, #state{data = JSON} = State) when is_map(JSON) ->
    {[{{<<"application">>, <<"json">>, []}, serve_json}], Req, State};
content_types_provided(Req, #state{data = File} = State) when is_binary(File) ->
    {[{cow_mimetypes:web(File), serve_file}], Req, State}.

generate_etag(Req, #state{component = Component, tag = Tag, data = File} = State)
  when is_binary(File) ->
    ETag = <<Component/binary, $_, Tag/binary, $_, File/binary>>,
    {{strong, ETag}, Req, State};
generate_etag(Req, #state{action = Action, component = Component, tag = Tag} = State)
  when Action =:= settings; Action =:= settingsForm ->
    ETag = <<Component/binary, $_, Tag/binary, $_, (atom_to_binary(Action, utf8))/binary>>,
    {{strong, ETag}, Req, State};
generate_etag(Req, #state{action = css, component = Component, tag = Tag, data = #{hash := Hash}} = State) ->
    ETag = <<Component/binary, $_, Tag/binary, $_, "css", $_, Hash/binary>>,
    {{strong, ETag}, Req, State}.

serve_css(Req, #state{data = CSSFile} = State) when is_binary(CSSFile) ->
    serve_css(Req, State#state{data = [CSSFile]});
serve_css(Req, #state{component = Component, tag = Tag, data = CSSData} = State) 
  when is_map(CSSData) ->
    {ok, CSSFiles} = ds_api_component:css(Component, Tag, CSSData),
    serve_files(Req, State#state{data = CSSFiles}).

serve_json(Req, #state{data = JSON} = State) when is_map(JSON) ->
    {jiffy:encode(JSON), Req, State}.

serve_file(Req, #state{data = File} = State) when is_binary(File) ->
    {fun (Socket, Transport) -> Transport:sendfile(Socket, File) end, Req, State}.

serve_files(Req, #state{data = Files} = State) when is_list(Files) ->
    SendFiles = fun (Socket, Transport) ->
                        lists:foreach(
                          fun (File) -> Transport:sendfile(Socket, File) end,
                          Files
                         )
                end,
    {SendFiles, Req, State}.
