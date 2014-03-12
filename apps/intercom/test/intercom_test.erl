-module (intercom_test).
-author("Mario Georgiev").

-compile (export_all).

-define(APPS, [compiler, syntax_tools, goldrush, lager, gproc, erlang_ale, doorlock, intercom]).
-define(HOST, "http://localhost:3000/intercom/1.0").

% etest macros
-include_lib ("etest/include/etest.hrl").
% etest_http macros
-include_lib ("etest_http/include/etest_http.hrl").

before_suite() ->
  [ ok = application:start(APP) || APP <- ?APPS].

after_suite() ->
  [ application:stop(APP) || APP <- ?APPS].

test_door_status() ->
  Resp = ?perform_get(?HOST ++ "/door"),
  lager:info("Door status: ~p", [Resp]),
  ?assert_status(200, Resp),
  ?assert_json_key(<<"status">>, Resp).

test_door_lock() ->
  Resp = ?perform_post(?HOST ++ "/door/lock"),
  ?assert_status(200, Resp),
  ?assert_json_value(<<"status">>, <<"locked">>, Resp).

test_door_unlock() ->
  Resp0 = ?perform_post(?HOST ++ "/door/unlock"),
  ?assert_status(200, Resp0),
  ?assert_json_values([{<<"status">>, <<"unlocked">>}, {<<"timeout">>, 5000}], Resp0),
  ?perform_post(?HOST ++ "/door/lock"),

  Resp1 = ?perform_post(?HOST ++ "/door/unlock", [], <<>>, [{timeout,  4000}]),
  lager:info("Door unlocked for 4000 ms: ~p", [Resp1]),
  ?assert_status(200, Resp1),
  %% TODO This test is broken, the etest-http doesn't post arguments as it should
  %% ?assert_json_values([{<<"status">>, <<"unlocked">>}, {<<"timeout">>, 4000}], Resp1),
  ?perform_post(?HOST ++ "/door/lock"),

  _Resp2 = ?perform_post(?HOST ++ "/door/unlock", [], <<>>, [{timeout, 20000}]),
  %% TODO This test is broken, the etest-http doesn't post arguments as it should
  %% ?assert_status(400, Resp2),
  ?perform_post(?HOST ++ "/door/lock").
