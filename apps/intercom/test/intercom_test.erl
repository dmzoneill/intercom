%%%-------------------------------------------------------------------
%%% @author Mario
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. III 2014 16:25
%%%-------------------------------------------------------------------
-module (intercom_test).
-author("Mario Georgiev").

-compile (export_all).

-define(APPS, [gproc, erlang_ale, doorlock, intercom]).
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
  ?assert_status(200, Resp),
  ?assert_json_key(<<"status">>, Resp).

test_door_unlock() ->
  Resp = ?perform_put(?HOST ++ "/door/unlock"),
  ?assert_status(200, Resp),
  ?assert_json_values([{<<"status">>, <<"unlocked">>}, {<<"timeout">>, 5000}], Resp).

test_door_unlock_with_timeout() ->
  Resp1 = ?perform_put(?HOST ++ "/door/unlock", [], <<>>, [{timeout,  4000}]),
  ?assert_status(200, Resp1),
  ?assert_json_values([{<<"status">>, <<"unlocked">>}, {<<"timeout">>, 4000}], Resp1),
  Resp2 = ?perform_put(?HOST ++ "/door/unlock", [], <<>>, [{timeout, 20000}]),
  ?assert_status(400, Resp2).

test_door_lock() ->
  Resp = ?perform_put(?HOST ++ "/door/lock"),
  ?assert_status(200, Resp),
  ?assert_json_value(<<"status">>, <<"locked">>, Resp).
