-module(eradiusd_util).

-export([start/0, init/0, test/2, auth/2, load_config/0]).

-include_lib("eradius/include/eradius_lib.hrl").
-include_lib("eradius/include/eradius_dict.hrl").
-include_lib("eradius/include/dictionary.hrl").

-define(CONFIG_FILE, "config.erl").

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
	case file:consult(?CONFIG_FILE) of
		{ok, Config} when is_list(Config) ->
			lists:foreach(fun(Line = {CfgType, Params}) ->
				io:format("Config line: ~p (~p)~n", [Line, CfgType]),
				case load_config(CfgType, Params) of
					{ok, Type, Cfg} ->
						io:format("Loaded ok: ~p (~p)~n", [Type, Cfg]);
					{error, Reason, Cfg} ->
						io:format("Failed to load: ~p (~p)~n", [Reason, Cfg])
				end
			end, Config);
		{error, Reason} ->
			io:format("Malformed config file ~p: ~p~n", [?CONFIG_FILE, file:format_error(Reason)])
	end.

load_config(bind, Cfg = [ListenIP, ListenPort]) when is_tuple(ListenIP) ->
	io:format("Binding RADIUS server to ~p:~p~n", [ListenIP, ListenPort]),
	supervisor:start_child(eradiusd_server_sup, [ListenIP, ListenPort]),
	eradius_server:trace_on(ListenIP, ListenPort),
	{ok, bind, Cfg};
load_config(bind, Cfg = [Address, ListenPort]) when is_list(Address) ->
	{ok, ListenIP} = inet_parse:address(Address),
	io:format("Binding RADIUS server to ~p:~p~n", [ListenIP, ListenPort]),
	supervisor:start_child(eradiusd_server_sup, [ListenIP, ListenPort]),
	eradius_server:trace_on(ListenIP, ListenPort),
	{ok, bind, Cfg};
load_config(ras, Cfg = [ListenIP, ListenPort, Secret]) when is_tuple(ListenIP) ->
	start_ras(ListenIP, ListenPort, Secret),
	{ok, ras, Cfg};
load_config(ras, Cfg = [Address, ListenPort, Secret]) when is_list(Address) ->
	{ok, ListenIP} = inet_parse:address(Address),
	start_ras(ListenIP, ListenPort, Secret),
	{ok, ras, Cfg};
load_config(realm, Cfg = [_Domain, _Required]) ->
	{ok, realm, Cfg};
load_config(ConfigType, Other) ->
	{error, badcfg, {ConfigType, Other}}.

start_ras(ListenIP, ListenPort, Secret) ->
	io:format("Starting RAS listener: ~p:~p~n", [ListenIP, ListenPort]),
	eradius_server:define_ras(ListenIP, ListenPort, Secret, {eradiusd_util, auth}).

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
