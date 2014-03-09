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
-define(HOST, "http://localhost:3000").

% etest macros
-include_lib ("etest/include/etest.hrl").
% etest_http macros
-include_lib ("etest_http/include/etest_http.hrl").

before_suite() ->
  [ ok = application:start(APP) || APP <- ?APPS].

after_suite() ->
  [ application:stop(APP) || APP <- ?APPS].

test_door_status() ->
  Resp = ?perform_get(?HOST ++ "/doorlock"),
  ?assert_status(200, Resp).

test_unlock_door() ->
  Resp = ?perform_put(?HOST ++ "/doorlock/unlock"),
  ?assert_status(200, Resp).

test_hello_world() ->
    Response = ?perform_get(?HOST ++ "/hello/world"),
    ?assert_status(200, Response),
    ?assert_body_contains("Hello", Response),
    ?assert_body("Hello World!", Response).
