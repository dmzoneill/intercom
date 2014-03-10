-module(intercom_callback).

-export([handle/2, handle_event/3]).

-include_lib("elli/include/elli.hrl").
-behaviour(elli_handler).

-define(API_VER, <<"1.0">>).
-define(JSON_RESP, {<<"Content-type">>, <<"application/json; charset=UTF-8">>}).

handle(Req, _Args) ->
    %% Delegate to our handler function
    handle(Req#req.method, elli_request:path(Req), Req).

handle('GET', [<<"intercom">>, ?API_VER, <<"door">>], _Req) ->
  {ok, Status} = doorlock:status(),
  {ok, [?JSON_RESP], jiffy:encode({Status})};

handle('PUT', [<<"intercom">>, ?API_VER, <<"door">>, <<"unlock">>], Req) ->
  Timeout = elli_request:get_arg(<<"timeout">>, Req, elli_request:post_arg(<<"timeout">>, Req)),
  Resp =
    case Timeout of
      undefined ->
        doorlock:unlock();
      _ ->
        T = binary_to_integer(Timeout),
        doorlock:unlock(T)
    end,
  case Resp of
    {ok, Status} ->
      {ok, [?JSON_RESP], jiffy:encode({Status})};
    {error, Msg} ->
      {400, [], list_to_binary(Msg)}
  end;

handle('PUT', [<<"intercom">>, ?API_VER, <<"door">>, <<"lock">>], _Req) ->
  {ok, Status} = doorlock:lock(),
  {ok, [?JSON_RESP], jiffy:encode({Status})};

handle(_, _, _Req) ->
    {404, [], <<"Not Found">>}.

%% @doc: Handle request events, like request completed, exception
%% thrown, client timeout, etc. Must return 'ok'.
handle_event(_Event, _Data, _Args) ->
    ok.
