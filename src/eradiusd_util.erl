-module(eradiusd_util).

-export([init/0]).

init() ->
	eradius_server:create_tables([node()]),
	load_config(),
	ok.

load_config() ->
	% this should be from a config file..
	io:format("Loading config~n"),
	eradius_server:define_ras({127,0,0,1}, 1812, "secret", {server, test}).
