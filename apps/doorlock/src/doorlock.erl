%%%-------------------------------------------------------------------
%%% @author Mario
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. III 2014 15:07
%%%-------------------------------------------------------------------
-module(doorlock).
-author("Mario").

-behaviour(gen_server).

%% API
-export([
  start_link/0,
  unlock/0,
  unlock/1,
  lock/0,
  status/0
]).

%% gen_server
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-define(SERVER, ?MODULE).

-define(PIN, 22).
-define(LOCKED, 0).
-define(UNLOCKED, 1).
-define(TIMEOUT, 5000).
-define(MIN_TIMEOUT, 3000). %% TODO: enforce
-define(MAX_TIMEOUT, 15000).

-record(state, {
  status = locked,
  timeout = ?TIMEOUT,
  timeref,
  pin_state
}).

unlock() ->
  gen_server:call(?MODULE, {unlock, ?TIMEOUT}).

unlock(Timeout) when Timeout =< ?MAX_TIMEOUT ->
  gen_server:call(?MODULE, {unlock, Timeout});

unlock(_) ->
  {error, "Timeout exceeds ?MAX_TIMEOUT"}.

lock() ->
  gen_server:call(?MODULE, lock).

status() ->
  gen_server:call(?MODULE, status).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init(_Args) ->
  {ok, _PinState} = gpio:start_link({?PIN, output}),
  State = handle_gpio(?LOCKED),
  {ok, State}.

handle_call({unlock, Timeout}, _From, State) ->
  case is_reference(State#state.timeref) of
    true  -> erlang:cancel_timer(State#state.timeref);
    false -> ok
  end,
  NewState = handle_gpio(?UNLOCKED),
  TimeRef = erlang:send_after(Timeout, self(), autolock),
  %% TODO: create token to prevent more than one person setting the door unlocked?
  UpdatedState = NewState#state{timeref = TimeRef},
  {reply, {ok, response(UpdatedState)}, UpdatedState};

handle_call(lock, _From, _State) ->
  NewState = handle_gpio(?LOCKED),
  {reply, {ok, response(NewState)}, NewState};

handle_call(status, _From, State) ->
  Reply = response(State),
  {reply, {ok, Reply}, State};

handle_call(_Request, _From, State) ->
  {noreply, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(autolock, _State) ->
  NewState = handle_gpio(?LOCKED),
  {noreply, NewState};

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  gpio:release(?PIN),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% private

remaining_ms(State) when is_reference(State#state.timeref) ->
  case T = erlang:read_timer(State#state.timeref) of
    false -> ?TIMEOUT;
    _     -> T
  end;

remaining_ms(_) ->
  ?TIMEOUT.

response(State) ->
  case State#state.status of
    locked   -> [{status, State#state.status}];
    unlocked ->
      Timeout = remaining_ms(State),
      [{status, State#state.status}, {timeout, Timeout}]
  end.

handle_gpio(Lock) ->
  PinState = gpio:write(?PIN, Lock),
  case Lock of
    ?LOCKED   -> #state{status = locked, pin_state = PinState};
    ?UNLOCKED -> #state{status = unlocked, pin_state = PinState}
  end.
