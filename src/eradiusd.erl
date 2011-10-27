-module(eradiusd).

-export([start/0, start_link/1]).

start() ->
	io:format("Starting eradiusd~n"),
	eradius_server:create_tables([node()]),
	load_config(),
	ok.

start_link([]) ->
	start().

load_config() ->
	% this should be from a config file..
	io:format("Loading config~n"),
	eradius_server:define_ras({127,0,0,1}, 1812, "secret", {server, test}).
