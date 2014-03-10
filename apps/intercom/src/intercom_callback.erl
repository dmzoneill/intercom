-module(intercom_callback).

-export([handle/2, handle_event/3]).

-include_lib("elli/include/elli.hrl").
-behaviour(elli_handler).

-define(JSON_RESP, {<<"Content-type">>, <<"application/json; charset=UTF-8">>}).

handle(Req, _Args) ->
    %% Delegate to our handler function
    handle(Req#req.method, elli_request:path(Req), Req).

handle('GET', [<<"doorlock">>], _Req) ->
  {ok, Status} = doorlock:status(),
  {ok, [?JSON_RESP], jiffy:encode({Status})};

handle('PUT', [<<"doorlock">>, <<"unlock">>], _Req) ->
  {ok, Status} = doorlock:unlock(),
  {ok, [?JSON_RESP], jiffy:encode({Status})};

handle('PUT', [<<"doorlock">>, <<"lock">>], _Req) ->
  {ok, Status} = doorlock:lock(),
  {ok, [?JSON_RESP], jiffy:encode({Status})};

handle('GET',[<<"hello">>, <<"world">>], _Req) ->
    %% Reply with a normal response. 'ok' can be used instead of '200'
    %% to signal success.
    {ok, [], <<"Hello World!">>};

handle(_, _, _Req) ->
    {404, [], <<"Not Found">>}.

%% @doc: Handle request events, like request completed, exception
%% thrown, client timeout, etc. Must return 'ok'.
handle_event(_Event, _Data, _Args) ->
    ok.
