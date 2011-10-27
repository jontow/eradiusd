-module(eradiusd_util).

-export([start/0, init/0, test/2, auth/2]).

-include_lib("eradius/include/eradius_lib.hrl").
-include_lib("eradius/include/eradius_dict.hrl").
-include_lib("eradius/include/dictionary.hrl").

-define(RAS_CONFIG, "ras_config.erl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% elegant start code stolen from lager: 
%% https://github.com/basho/lager/blob/master/src/lager.erl#L37 (thanks, andrew)
%%

start() -> start(eradiusd).

start(App) ->
    start_ok(App, application:start(App, permanent)).

start_ok(_App, ok) -> ok;
start_ok(_App, {error, {already_started, _App}}) -> ok;
start_ok(App, {error, {not_started, Dep}}) -> 
    ok = start(Dep),
    start(App);
start_ok(App, {error, Reason}) -> 
    erlang:error({app_start_failed, App, Reason}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init() ->
	eradius_server:create_tables([node()]),
	load_config(),
	ok.

load_config() ->
	% this should be from a config file..
	io:format("Loading config~n"),
	case file:consult(?RAS_CONFIG) of
		{ok, Config} when is_list(Config) ->
			lists:foreach(fun({ListenIP, ListenPort, Secret}) when is_tuple(ListenIP) ->
					io:format("RAS: ~p:~p (~p)~n", [ListenIP, ListenPort, Secret]),
					eradius_server:define_ras(ListenIP, ListenPort, Secret, {eradiusd_util, auth});
				({Address, ListenPort, Secret}) when is_list(Address) ->
					{ok, ListenIP} = inet_parse:address(Address),
					io:format("RAS: ~p:~p (~p)~n", [ListenIP, ListenPort, Secret]),
					eradius_server:define_ras(ListenIP, ListenPort, Secret, {eradiusd_util, auth});
				(_Other) ->
					io:format("Malformed config line: ~p~n", [Config])
			end, Config);
		{error, Reason} ->
			io:format("Malformed config file ~p: ~p~n", [?RAS_CONFIG, file:format_error(Reason)])
	end.

% Minimal (!) example of Access Request handler
test(#rad_pdu{}, #nas_prop{}) ->
    io:format("Test spawned~n"),
    #rad_accept{}.

auth(#rad_pdu{} = Pdu, #nas_prop{} = Nas) ->
    io:format("Auth spawned~n"),
    %io:format("Auth spawned: ~p~n", [Pdu#rad_pdu.cmd]),
    {request, Attrs} = Pdu#rad_pdu.cmd,
	%io:format("Attributes: ~p~n", [Attrs]),
    case lookup(?User_Name, Attrs) of
        {ok, User} ->
            case lookup(?User_Password, Attrs) of
                {ok, Pass} ->
					io:format("Request (PAP authentication) for ~p~n", [User]),
                    pap(User, Pass, Nas#nas_prop.secret, Pdu#rad_pdu.authenticator);
                false ->
                    case lookup(?CHAP_Password, Attrs) of
                        {ok, Chap_pass} ->
                            Challenge = case lookup(?CHAP_Challenge, Attrs) of
                                            {ok, Val} ->
                                                Val;
                                            false ->
                                                Pdu#rad_pdu.authenticator
                                        end,
							io:format("Request (CHAP authentication) for ~p~n", [User]),
                            chap(User, list_to_binary(Chap_pass), Challenge);
                        false ->
                            #rad_reject{}
                    end
            end;
        false ->
			io:format("Username lookup failed: ~p~n", [?User_Name]),
            #rad_reject{}
    end.

pap(User, Req_pass, Secret, Auth) ->
	%io:format("pap() called with: (~p), (~p), (~p), (~p)~n", [User, Req_pass, Secret, Auth]),
    case get_user(User) of
        {ok, Passwd} ->
			%io:format("pap() got password back: ~p:~p:~p~n", [User, Passwd, Auth]),
            Enc_pass = eradius_lib:mk_password(Secret, Auth, Passwd),
            if Enc_pass == Req_pass ->
					io:format("pap() sending Access-Accept for ~p~n", [User]),
                    #rad_accept{};
               true ->
                    io:format("PAP~p~n",[{User, Req_pass, Secret, Auth, Enc_pass}]),
                    #rad_reject{}
            end;
        false ->
            #rad_reject{}
    end.

chap(User, <<Chap_id, Chap_pass/binary>>, Chap_challenge) ->
    case get_user(User) of
        {ok, Passwd} ->
            Enc_pass = erlang:md5([Chap_id, Passwd, Chap_challenge]),
            if Enc_pass == Chap_pass ->
                    #rad_accept{};
               true ->
                    #rad_reject{}
            end;
        false ->
             #rad_reject{}
    end.

lookup(Key, [{Key, Val}|_T]) ->
    {ok, Val};
lookup(Key, [_|T]) ->
	%io:format("Lookup key: ~p // tail: ~p~n", [Key, T]),
    lookup(Key, T);
lookup(_, []) ->
    false.

get_user(User) ->
	%io:format("Reading passwords.txt~n"),
	{ok, Bin} = file:read_file("passwords.txt"),
	Lines = binary:split(Bin, <<"\n">>, [global]),
	Rows = lists:map(fun(Line) -> binary:split(Line, <<":">>, [global]) end, Lines),
	%io:format("Calling get_pass(~p // ~p)~n", [User, Rows]),
	case get_pass(User, Rows) of
		{ok, User, Pass} ->
			%io:format("get_pass() succeeded, returning~n"),
			{ok, Pass};
		false ->
			false
	end.

get_pass(User, [[User, Pass, _]|_T]) ->
	%io:format("get_pass() successful match on ~p // ~p~n", [User, Pass]),
	{ok, User, Pass};
get_pass(User, [_H|T]) ->
	%o:format("get_pass() trying match on ~p // ~p~n", [User, H]),
	get_pass(User, T);
get_pass(_User, []) ->
	%io:format("get_pass() no match on ~p~n", [User]),
	false.
