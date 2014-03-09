-module(intercom_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    application:start(intercom).

start(_StartType, _StartArgs) ->
    intercom_sup:start_link().

stop(_State) ->
    ok.
