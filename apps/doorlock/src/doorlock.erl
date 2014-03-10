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

-record(state, {
  status = locked,
  timeout,
  timeref,
  pin_state
}).

unlock() ->
  {ok, Timeout} = application:get_env(?MODULE, timeout),
  gen_server:call(?MODULE, {unlock, Timeout}).

unlock(Timeout) ->
  {ok, Min} = application:get_env(?MODULE, min_timeout),
  {ok, Max} = application:get_env(?MODULE, max_timeout),
  case Timeout of
    T when (T >= Min) and (T =< Max) ->
      gen_server:call(?MODULE, {unlock, T});
    _ ->
      {error, lists:concat(["Timeout is less than ", Min, " or exceeds ", Max])}
  end.

lock() ->
  gen_server:call(?MODULE, lock).

status() ->
  gen_server:call(?MODULE, status).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init(_Args) ->
  {ok, Pin} = application:get_env(?MODULE, pin),
  {ok, _PinState} = gpio:start_link({Pin, output}),
  {ok, Locked} = application:get_env(?MODULE, locked),
  State = handle_gpio(Locked),
  {ok, State}.

handle_call({unlock, Timeout}, _From, State) ->
  case is_reference(State#state.timeref) of
    true  -> erlang:cancel_timer(State#state.timeref);
    false -> ok
  end,
  {ok, Unlocked} = application:get_env(?MODULE, unlocked),
  NewState = handle_gpio(Unlocked),
  TimeRef = erlang:send_after(Timeout, self(), autolock),
  %% TODO: create token to prevent more than one person setting the door unlocked?
  UpdatedState = NewState#state{timeref = TimeRef},
  {reply, {ok, response(UpdatedState)}, UpdatedState};

handle_call(lock, _From, _State) ->
  {ok, Locked} = application:get_env(?MODULE, locked),
  NewState = handle_gpio(Locked),
  {reply, {ok, response(NewState)}, NewState};

handle_call(status, _From, State) ->
  Reply = response(State),
  {reply, {ok, Reply}, State};

handle_call(_Request, _From, State) ->
  {noreply, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(autolock, _State) ->
  {ok, Locked} = application:get_env(?MODULE, locked),
  NewState = handle_gpio(Locked),
  {noreply, NewState};

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  {ok, Pin} = application:get_env(?MODULE, pin),
  gpio:release(Pin),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% private

remaining_ms(State) when is_reference(State#state.timeref) ->
  {ok, Timeout} = application:get_env(?MODULE, timeout),
  case T = erlang:read_timer(State#state.timeref) of
    false -> Timeout;
    _     -> T
  end;

remaining_ms(_) ->
  {ok, Timeout} = application:get_env(?MODULE, timeout),
  Timeout.

response(State) ->
  case State#state.status of
    locked   -> [{status, State#state.status}];
    unlocked ->
      Timeout = remaining_ms(State),
      [{status, State#state.status}, {timeout, Timeout}]
  end.

handle_gpio(Lock) ->
  {ok, Pin} = application:get_env(?MODULE, pin),
  {ok, Locked} = application:get_env(?MODULE, locked),
  {ok, Unlocked} = application:get_env(?MODULE, unlocked),
  PinState = gpio:write(Pin, Lock),
  case Lock of
    Locked   -> #state{status = locked, pin_state = PinState};
    Unlocked -> #state{status = unlocked, pin_state = PinState}
  end.
