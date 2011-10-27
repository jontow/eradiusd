-module(eradiusd_util).

-export([init/0, test/2, auth/2]).

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
	eradius_server:define_ras({127,0,0,1}, 1812, "secret", {eradiusd_util, auth}).

% Minimal (!) example of Access Request handler
test(#rad_pdu{}, #nas_prop{}) ->
    io:format("Test spawned~n"),
    #rad_accept{}.

auth(#rad_pdu{} = Pdu, #nas_prop{} = Nas) ->
    io:format("Auth spawned~n"),
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
            Enc_pass = eradius_lib:mk_password(Secret, Auth, Passwd),
            Req_pass1 = list_to_binary(Req_pass),
            if Enc_pass == Req_pass1 ->
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

lookup(Key, [{#attribute{id = Key}, Val}|_T]) ->
    {ok, Val};
lookup(Key, [_|T]) ->
    lookup(Key, T);
lookup(_, []) ->
    false.

get_user(User) ->
	io:format("Reading passwords.txt~n"),
	{ok, Bin} = file:read_file("passwords.txt"),
	Lines = binary:split(Bin, <<"\n">>),
	Rows = lists:map(fun(Line) -> binary:split(Line, <<":">>) end, Lines),
	io:format("Calling get_pass()~n"),
	get_pass(Rows, User).

get_pass([_H = <<User, Pass, _>>|_T], User) ->
	io:format("Trying match on ~p:~p~n", [User, Pass]),
	{ok, User, Pass};
get_pass([H|T], User) ->
	io:format("Trying match on ~p~n", [H]),
	get_pass(T, User);
get_pass([], _User) ->
	false.

% Passfile reading, should maybe go like this:
%{ok, Bin} = file:read_file("filename")
%Lines = binary:split(Bin, <<"\n">>)
%Rows = lists:map(fun(Line) -> binary:split(Line, <<":">>) end, Lines)
%22:22 <@Vagabond> that'll give you a list of 3 element lists
%22:22 <@Vagabond> like [[<<"user">>, <<"pass">>, <<"package">>], ...]
%22:23 <@Vagabond> actually, if you used list_to_tuple, you could then use lists:keysearch to search for the 
%                  user
