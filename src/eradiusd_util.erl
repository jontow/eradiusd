-module(eradiusd_util).

-export([start/0, init/0, test/2, auth/2]).

-include_lib("eradius/include/eradius_lib.hrl").
-include_lib("eradius/include/eradius_dict.hrl").
-include_lib("eradius/include/dictionary.hrl").


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
	eradius_server:define_ras({127,0,0,1}, 1812, "secret", {eradiusd_util, auth}).

% Minimal (!) example of Access Request handler
test(#rad_pdu{}, #nas_prop{}) ->
    io:format("Test spawned~n"),
    #rad_accept{}.

auth(#rad_pdu{} = Pdu, #nas_prop{} = Nas) ->
    io:format("Auth spawned: ~p~n", [Pdu#rad_pdu.cmd]),
    {request, Attrs} = Pdu#rad_pdu.cmd,
	io:format("Attributes: ~p~n", [Attrs]),
    case lookup(?User_Name, Attrs) of
        {ok, User} ->
            case lookup(?User_Password, Attrs) of
                {ok, Pass} ->
					io:format("pap authentication~n"),
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
							io:format("chap authentication~n"),
                            chap(User, list_to_binary(Chap_pass), Challenge);
                        false ->
                            #rad_reject{}
                    end
            end;
        false ->
			io:format("lookup username failed~n"),
            #rad_reject{}
    end.

pap(User, Req_pass, Secret, Auth) ->
    case get_user(User) of
        {ok, Passwd} ->
			io:format("pap() got password back: ~p:~p:~p~n", [User, Passwd, Auth]),
            Enc_pass = eradius_lib:mk_password(Secret, Auth, Passwd),
            Req_pass1 = list_to_binary(Req_pass),
			io:format("Pass compare: ~p == ~p~n", [Enc_pass, Req_pass1]),
            if Enc_pass == Req_pass1 ->
					io:format("pap() sending Access-Accept~n"),
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
	io:format("Lookup key: ~p // tail: ~p~n", [Key, T]),
    lookup(Key, T);
lookup(_, []) ->
    false.

get_user(User) ->
	io:format("Reading passwords.txt~n"),
	{ok, Bin} = file:read_file("passwords.txt"),
	Lines = binary:split(Bin, <<"\n">>),
	Rows = lists:map(fun(Line) -> binary:split(Line, <<":">>, [global]) end, Lines),
	io:format("Calling get_pass(~p // ~p)~n", [User, Rows]),
	case get_pass(User, Rows) of
		{ok, User, Pass} ->
			io:format("get_pass() succeeded, returning~n"),
			{ok, Pass};
		false ->
			false
	end.

get_pass(User, [[User, Pass, _]|_T]) ->
	io:format("get_pass() successful match on ~p // ~p~n", [User, Pass]),
	{ok, User, Pass};
get_pass(User, [H|T]) ->
	io:format("get_pass() trying match on ~p // ~p~n", [User, H]),
	get_pass(T, User);
get_pass(User, []) ->
	io:format("get_pass() no match on ~p~n", [User]),
	false.
