-module(doorlock_test).
-author("Mario Georgiev").

-compile (export_all).

-define(APPS, [gproc, erlang_ale, doorlock]).

% etest macros
-include_lib ("etest/include/etest.hrl").

before_suite() ->
  [ ok = application:start(APP) || APP <- ?APPS].

after_suite() ->
  [ application:stop(APP) || APP <- ?APPS].

test_lock() ->
  Resp = doorlock:lock(),
  ?assert_equal({ok, [{status, locked}]}, Resp).

test_unlock() ->
  RespU = doorlock:unlock(),
  ?assert_equal({ok, [{status, unlocked}, {timeout, 5000}]}, RespU),
  ok = timer:sleep(5500),
  RespS = doorlock:status(),
  ?assert_equal({ok, [{status, locked}]}, RespS).

test_lock_while_unlocked() ->
  RespU = doorlock:unlock(),
  ?assert_equal({ok, [{status, unlocked}, {timeout, 5000}]}, RespU),
  Resp = doorlock:lock(),
  ?assert_equal({ok, [{status, locked}]}, Resp).

test_unlock_custom_interval() ->
  RespU = doorlock:unlock(7000),
  ?assert_equal({ok, [{status, unlocked}, {timeout, 7000}]}, RespU),
  ok = timer:sleep(1500),
  RespS1 = doorlock:status(),
  {ok, [{status, unlocked}, {timeout, Timeout}]} = RespS1,
  ?assert_equal(true, Timeout =< 5500),
  ok = timer:sleep(5500),
  RespS2 = doorlock:status(),
  ?assert_equal({ok, [{status, locked}]}, RespS2).

test_unlock_error() ->
  {error, _Msg} = doorlock:unlock(20000),
  ?assert_equal(true, true).
