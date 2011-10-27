-module(eradiusd_util).

-export([init/0]).

-include_lib("eradius/include/eradius_lib.hrl").
-include_lib("eradius/include/eradius_dict.hrl").
-include_lib("eradius/include/dictionary.hrl").

init() ->
	eradius_server:create_tables([node()]),
	load_config(),
	ok.

load_config() ->
	% this should be from a config file..
	io:format("Loading config~n"),
	eradius_server:define_ras({127,0,0,1}, 1812, "secret", {eradiusd_util, test}).

% Minimal (!) example of Access Request handler
test(#rad_pdu{}, #nas_prop{}) ->
    io:format("Test spawned~n"),
    #rad_accept{}.
