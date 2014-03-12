-module(intercom_callback).
-author("Mario Georgiev").

-export([handle/2, handle_event/3]).

-include_lib("elli/include/elli.hrl").
-behaviour(elli_handler).

-define(API_VER, <<"1.0">>).
-define(JSON_RESP, {<<"Content-type">>, <<"application/json; charset=UTF-8">>}).

handle(Req, _Args) ->
    handle(Req#req.method, elli_request:path(Req), Req).

handle('GET', [<<"intercom">>, ?API_VER, <<"door">>], _Req) ->
  {ok, Status} = doorlock:status(),
  {ok, [?JSON_RESP], jiffy:encode({Status})};

handle('POST', [<<"intercom">>, ?API_VER, <<"door">>, <<"lock">>], _Req) ->
  {ok, Status} = doorlock:lock(),
  json_resp(ok, {Status});

handle('POST', [<<"intercom">>, ?API_VER, <<"door">>, <<"unlock">>], Req) ->
  Timeout = elli_request:post_arg(<<"timeout">>, Req),
  Resp = unlock(Timeout),
  case Resp of
    {ok, Status} ->
      json_resp(ok, {Status});
    {error, Msg} ->
      {400, [], list_to_binary(Msg)}
  end;

handle(_, _, _Req) ->
    {404, [], <<"Not Found">>}.

%% @doc: Handle request events, like request completed, exception
%% thrown, client timeout, etc. Must return 'ok'.
handle_event(_Event, _Data, _Args) ->
    ok.

%% private

json_resp(Code, Msg) ->
  {Code, [?JSON_RESP], jiffy:encode(Msg)}.

unlock(Timeout) when is_binary(Timeout) ->
  doorlock:unlock(binary_to_integer(Timeout));

unlock(Timeout) when is_integer(Timeout) ->
  doorlock:unlock(Timeout);

unlock(_Timeout) ->
  doorlock:unlock().
