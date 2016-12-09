-module(inventory_db).
-behaviour(gen_server).

%% API

-export([ start_link/0
        , add/1
        , clear/0
        , delete/1
        , get/1
        , index/0
        ]).

%% gen_server callbacks

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Macros and records

-define(SERVER, ?MODULE).

%%%---------------------------------------------------------------------
%%% API
%%%---------------------------------------------------------------------

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

add(Sock) ->
  gen_server:call(?SERVER, {add, Sock}).

clear() ->
  gen_server:call(?SERVER, clear).

delete(Name) ->
  gen_server:cast(?SERVER, {delete, Name}).

get(Name) ->
  gen_server:call(?SERVER, {get, Name}).

index() ->
  gen_server:call(?SERVER, index).

%%%---------------------------------------------------------------------
%%% gen_server callbacks
%%%---------------------------------------------------------------------

init([]) ->
  process_flag(trap_exit, true),
  {ok, #{}}.

handle_call({add, Map}, _From, State) ->
  Name = maps:get(name, Map),
  NewState = maps:put(Name, Map, State),
  {reply, ok, NewState};
handle_call(clear, _From, _State) ->
  {reply, ok, #{}};
handle_call({get, Name}, _From, State) ->
  Reply = case maps:get(Name, State, not_found) of
            not_found -> error;
            Sock -> Sock
          end,
  {reply, Reply, State};
handle_call(index, _From, State) ->
  {reply, State, State};
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast({delete, Name}, State) ->
  NewState = maps:remove(Name, State),
  {noreply, NewState};
handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Local variables:
%% mode: erlang
%% erlang-indent-level: 2
%% indent-tabs-mode: nil
%% fill-column: 72
%% coding: latin-1
%% End:
