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

	case eradiusd_util:load_config() of
		{ok, Bind, Ras, Realms} ->
			eradiusd_util:start_ras(Ras, Realms),
			eradiusd_util:init(),
    		eradiusd_sup:start_link(Bind);
		{error, Reason} ->
			io:format("Couldn't load config: ~p~n", [Reason]),
			stop([])
	end.

stop(_State) ->
    ok.
