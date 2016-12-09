-module(inventory_app).
-behaviour(application).

%% Application callbacks

-export([ start/2
        , stop/1
        , reload_routing/0
        ]
       ).

%% Macros

-define(
   ROUTES,
   [ { "/socks[/:name]"
     , inventory_handler
     , []
     }
   ]
  ).

%%%---------------------------------------------------------------------
%%% API
%%%---------------------------------------------------------------------

start(_StartType, _StartArgs) ->
  Dispatch = cowboy_router:compile([{'_', ?ROUTES}]),
  {ok, Port} = application:get_env(payment, port),
  {ok, _} =
    cowboy:start_http(
      inventory_http_listener,
      100,
      [{port, Port}],
      [{env, [{dispatch, Dispatch}]}]
     ),
  sync:go(),
  sync:onsync(fun (_) -> reload_routing() end),
  inventory_sup:start_link().

stop(_State) ->
  ok.

reload_routing() ->
  cowboy:set_env( inventory_http_listener
                , dispatch
                , cowboy_router:compile([{'_', ?ROUTES}])
                ).

%% Local variables:
%% mode: erlang
%% erlang-indent-level: 2
%% indent-tabs-mode: nil
%% fill-column: 72
%% coding: latin-1
%% End:
