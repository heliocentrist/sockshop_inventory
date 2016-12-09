-module(inventory_sup).
-behaviour(supervisor).

%% API

-export([start_link/0]).

%% Supervisor callbacks

-export([init/1]).

%% Macros

-define(SERVER, ?MODULE).

%%%---------------------------------------------------------------------
%%% API
%%%---------------------------------------------------------------------

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%---------------------------------------------------------------------
%%% Supervisor callbacks
%%%---------------------------------------------------------------------

init([]) ->
    {ok, { #{ strategy => one_for_all
            , intensity => 0
            , period => 1
            }
         , [ #{ id => inventory_db
              , start => {inventory_db, start_link, []}
              }
           ]
         }
    }.

%% Local variables:
%% mode: erlang
%% erlang-indent-level: 2
%% indent-tabs-mode: nil
%% fill-column: 72
%% coding: latin-1
%% End:
