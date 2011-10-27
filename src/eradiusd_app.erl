-module(eradiusd_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    eradiusd_sup:start_link(),
	eradiusd:start().

stop(_State) ->
    ok.
