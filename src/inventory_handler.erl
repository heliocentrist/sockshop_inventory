-module(inventory_handler).

%% Resource functions

-export([ init/3
        , rest_init/2
        , allowed_methods/2
        , resource_exists/2
        , content_types_accepted/2
        , allow_missing_post/2
        , content_types_provided/2
        , to_json/2
        , from_json/2
        ]).

%% Macros and records

-record(state, {mode, name, sock}).

%%%---------------------------------------------------------------------
%%% Resource functions
%%%---------------------------------------------------------------------

init(_Transport, _Req, _) ->
  {upgrade, protocol, cowboy_rest}.

rest_init(Req, []) ->
  {Name, Req2} = cowboy_req:binding(name, Req),
  Mode = case Name of
           undefined -> collection;
           _         -> document
         end,
  {ok, Req2, #state{mode = Mode, name = Name}}.

allowed_methods(Req, State) ->
  {[<<"GET">>, <<"POST">>], Req, State}.

resource_exists(Req, State = #state{mode = Mode, name = Name}) ->
  case Mode of
    collection ->
      {true, Req, State};
    document ->
      case inventory_db:get(Name) of
        error ->
          {false, Req, State};
        Sock ->
          {true, Req, State#state{sock = Sock}}
      end
  end.

content_types_accepted(Req, State) ->
  {[{{<<"application">>, <<"json">>, '*'}, from_json}], Req, State}.

allow_missing_post(Req, State) ->
  {true, Req, State}.

content_types_provided(Req, State) ->
  {[{{<<"application">>, <<"json">>, '*'}, to_json}], Req, State}.

to_json(Req, State = #state{mode = Mode, sock = Sock}) ->
  case Mode of
    collection ->
      Index = inventory_db:index(),
      Json = jsx:encode(Index),
      {Json, Req, State};
    document ->
      Json = jsx:encode(Sock),
      {Json, Req, State}
  end.

from_json(Req, State) ->
  {ok, Body, Req2} = cowboy_req:body(Req),
  Sock = jsx:decode(Body, [{labels, atom}, return_maps]),
  Name = maps:get(name, Sock),
  inventory_db:add(Sock),
  {Base, Req3} = cowboy_req:path(Req2),
  Url = <<Base/bytes, "/", Name/bytes>>,
  {{true, Url}, Req3, State}.

%% Local variables:
%% mode: erlang
%% erlang-indent-level: 2
%% indent-tabs-mode: nil
%% fill-column: 72
%% coding: latin-1
%% End:
