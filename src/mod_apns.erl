%% Apple Push Notification Service for Ejabberd
%% Created: 07/09/2015 by mrDoctorWho
%% Forked: 11/02/2016 by joanlopez
%% License: MIT/X11

-module(mod_apns).
-author("mrDoctorWho").

-include("ejabberd.hrl").
-include("logger.hrl").
-include("jlib.hrl").
-include("mod_muc_room.hrl").

-behaviour(gen_mod).

-define(NS_APNS, "https://apple.com/push"). %% I hope Apple doesn't mind.

-export([start/2, stop/1, message/3, muc_message/5, iq/3, mod_opt_type/1]).

-define(Timeout, 10000).
 
% partially done by uwe-arzt.de
send_payload(Host, Payload, Token) ->
	Address = gen_mod:get_module_opt(Host, ?MODULE, address, fun(V) -> binary_to_list(V) end, undefined),
	Port = gen_mod:get_module_opt(Host, ?MODULE, port, fun(V) -> V end, undefined),
	Cert = gen_mod:get_module_opt(Host, ?MODULE, certfile, fun(V) -> binary_to_list(V) end, undefined),
	Keyfile = gen_mod:get_module_opt(Host, ?MODULE, keyfile, fun(V) -> binary_to_list(V) end, undefined),
	Password = gen_mod:get_module_opt(Host, ?MODULE, password, fun(V) -> binary_to_list(V) end, undefined),

	case Keyfile of
                undefined ->
			Options = [{certfile, Cert}, {password, Password}, {mode, binary}]; %, {verify, verify_none}
		_ ->
			Options = [{certfile, Cert}, {keyfile, Keyfile}, {mode, binary}]
	end,

	case ssl:connect(Address, Port, Options, ?Timeout) of
		{ok, Socket} ->
			PayloadBin = list_to_binary(Payload),
			PayloadLength = size(PayloadBin),
			TokenNum = erlang:binary_to_integer(Token, 16),
                        TokenBin = <<TokenNum:32/integer-unit:8>>,
                        Packet = <<
                                0:8,
                                32:16/big,
                                TokenBin/binary,
                                PayloadLength:16/big,
                                PayloadBin/binary
                        >>,
			ssl:send(Socket, Packet),
			ssl:close(Socket),
			?DEBUG("mod_apns: Successfully sent payload:~p to the APNS server", [Payload]),
			ok;
		{error, Reason} ->
			?ERROR_MSG("mod_apns: Unable to connect to the APNS server: ~p", [Reason]),
			Reason	
	end.

create_json(List1, List2) ->
	lists:append(["{\"aps\":{", create_keyvalue(List1), "}, ", create_keyvalue(List2), "}"]).

create_keyvalue([Head]) ->
	create_pair(Head);
create_keyvalue([Head|Tail]) ->
	lists:append([create_pair(Head), ",", create_keyvalue(Tail)]).
 
create_pair({Key, Value}) ->
	case is_numeric(Value) of
    		false ->
			case string:substr(Value,1,1) of
				"{" ->
					lists:append([add_quotes(atom_to_list(Key)), ":", Value]);
				_ ->
					lists:append([add_quotes(atom_to_list(Key)), ":", add_quotes(Value)])

			end;
		true ->
			lists:append([add_quotes(atom_to_list(Key)), ":", Value])
	end.

add_quotes(String) ->
	lists:append(["\"", String, "\""]).

get_offlinemsg_module(Server) ->
    case gen_mod:is_loaded(Server, mod_offline) of
      true -> mod_offline;
      false -> none
    end.

is_numeric(L) ->
    Float = (catch erlang:list_to_float(L)),
    Int = (catch erlang:list_to_integer(L)),
    is_number(Float) orelse is_number(Int).

message(From, To, Packet) ->
	Type = xml:get_tag_attr_s(<<"type">>, Packet),
	?DEBUG("Offline message ~s, type: ~s", [From, Type]),
	case binary_to_list(Type) of 
		"normal" -> ok;
		"chat" ->
			%% Strings
			JFrom = jlib:jid_to_string(From#jid{user = From#jid.user, server = From#jid.server, resource = <<"">>}),
			JTo = jlib:jid_to_string(To#jid{user = To#jid.user, server = To#jid.server, resource = <<"">>}),
			ToUser = To#jid.user,
			ToServer = To#jid.server,
			LUser = jlib:nameprep(ToUser),
			LServer = jlib:nameprep(ToServer),
			FromName = get_name(From#jid.user, user, LServer),
			Body = xml:get_path_s(Packet, [{elem, <<"body">>}, cdata]),

			%% Checking subscription
			{Subscription, _Groups} = 
				ejabberd_hooks:run_fold(roster_get_jid_info, ToServer, {none, []}, [ToUser, ToServer, From]),
			case Subscription of
				both ->
					case Body of
						<<>> -> ok;
						_ ->
							send_push_to(JTo, FromName, LServer, ToServer, chat)
					end;
				_ -> ok
			end
	end.


muc_message(Stanza, MUCState, RoomJID, FromJID, FromNick) ->
	Type = xml:get_tag_attr_s(<<"type">>, Stanza),
	ToServer = FromJID#jid.server,
	LServer = jlib:nameprep(ToServer),
	Room = jlib:jid_to_string(RoomJID#jid{user = RoomJID#jid.user, server = RoomJID#jid.server, resource = <<"">>}),
	RoomName = get_name(Room, muc, LServer),
	?DEBUG("Roomname: ~p~n",[RoomName]),

	_LISTUSERS = lists:map(
        	fun({_LJID, Info}) ->
	        	list_to_binary(binary_to_list(Info#user.jid#jid.luser) ++ "@" ++ binary_to_list(ToServer))
        	end,
        	dict:to_list(MUCState#state.users)
    	),

    	_AFILLIATIONS = lists:map(
        	fun({{Uname, _Domain, _Res}, _Stuff}) ->
            		list_to_binary(binary_to_list(Uname) ++ "@" ++ binary_to_list(ToServer))
        	end,
        	dict:to_list(MUCState#state.affiliations)
    	),

    	_OFFLINE = lists:subtract(_AFILLIATIONS, _LISTUSERS),
	
	case binary_to_list(Type) of
		"groupchat" ->
			send_push_to(_OFFLINE, RoomName, LServer, ToServer, groupchat),
            		Stanza;
        	true ->
            		Stanza
    	end.

%% Send Push Method
send_push_to([], _, _, _, _) -> ok;
send_push_to([H|T], Name, LServer, ToServer, Type) -> 
	send_push_to(H, Name, LServer, ToServer, Type),
	send_push_to(T, Name, LServer, ToServer, Type);
send_push_to(UserJID, Name, LServer, ToServer, Type) ->
	?DEBUG("Sending push to: ~p~n",[UserJID]),
	case ejabberd_odbc:sql_query(LServer,
					[<<"select token from apns_users where "
						"user='">>,
       						UserJID, <<"';">>])
	of
                                        
		{selected, [<<"token">>], [[Token]]} ->
			if
				Token /= null ->
					Msg = generate_push_msg(Type, Name),
					Args = [{destination, binary_to_list(UserJID)}],
					JSON = create_json(Msg, Args),
					send_payload(ToServer, JSON, Token);
				true ->
					?DEBUG("No existing key for this user - maybe Android?",[])
			end;

		{selected, [<<"token">>], []} ->
			?DEBUG("No existing key for this user - maybe Android?",[])
	end.

generate_push_msg(chat, Name) -> [{alert, "{\"loc-key\":\"push_new_message\",\"loc-args\":[\"" ++ binary_to_list(Name) ++ "\"]}"}, {sound, "default"}];
generate_push_msg(groupchat, Name) -> [{alert, "{\"loc-key\":\"push_new_muc_message\",\"loc-args\":[\"" ++ binary_to_list(Name)  ++ "\"]}"}, {sound, "default"}].

get_name(Jid, Type, LServer) ->
	case Type of
		muc ->
			case ejabberd_odbc:sql_query(LServer,
                      		[<<"select name from muc where "
	                              "jid='">>,
                                       Jid, <<"';">>])
       			 of
				{selected, [<<"name">>], [[Name]]} ->	
					Name;
				true ->
					list_to_binary("default")
			end;

		user ->
			case ejabberd_odbc:sql_query(LServer,
                                [<<"select fn from vcard_search where "
                                      "username='">>,
                                       Jid, <<"';">>])
                         of
                                {selected, [<<"fn">>], [[Name]]} ->
                                        Name;
                                true ->
                                       list_to_binary("default")
                        end;
		true ->
			list_to_binary("default")
	end.


iq(#jid{user = User, server = Server} = From, To, #iq{type = Type, sub_el = SubEl} = IQ) ->
	LUser = jlib:nodeprep(User),
	LServer = jlib:nameprep(Server),
	JUser = jlib:jid_to_string(From#jid{user = From#jid.user, server = From#jid.server, resource = <<"">>}),

	{MegaSecs, Secs, _MicroSecs} = now(),
	TimeStamp = MegaSecs * 1000000 + Secs,

	Token = xml:get_tag_cdata(xml:get_subtag(SubEl, <<"token">>)),
	

	%% INSERT CASE
	F = fun() ->  ejabberd_odbc:sql_query(LServer,
			    [<<"insert into apns_users(user, token, last_seen) "
			       "values ('">>,
			     JUser, <<"', '">>, Token, <<"', '">>, integer_to_list(TimeStamp), <<"');">>]) 
	end,

	%% UPDATE LAST_SEEN CASE
	F2 = fun() ->	ejabberd_odbc:sql_query(LServer,
				[<<"update apns_users set "
				"last_seen='">>,
				integer_to_list(TimeStamp), <<"' where "
				"user='">>,
				JUser, <<"';">>])
  	end,

	case ejabberd_odbc:sql_query(LServer,
								[<<"select token from apns_users where "
									"user='">>,
									JUser, <<"';">>])
									of

		%% User exists
		{selected, [<<"token">>], [[Key]]} ->
			if
				Token /= Key ->
					%% UPDATE TOKEN CASE
					F3 = fun() -> ejabberd_odbc:sql_query(LServer,
									[<<"update apns_users set "
									"token='">>,
									Token, <<"' where "
									"user='">>,
									JUser, <<"';">>])
					end,
					ejabberd_odbc:sql_transaction(LServer, F3),
					?DEBUG("mod_apns: Updating key for user ~s@~s", [LUser, LServer]);

				true ->
					%% UPDATE TIMESTAMP CASE
					ejabberd_odbc:sql_transaction(LServer, F2),
					?DEBUG("mod_apns: Updating timestamp for user ~s@~s", [LUser, LServer])
			end;

		%% User does not exists
		{selected, [<<"token">>], []} ->
			ejabberd_odbc:sql_transaction(LServer, F),
			?DEBUG("mod_apns: Registered new token ~s@~s:~s", [LUser, LServer, Token])
		end,
	
	IQ#iq{type=result, sub_el=[]}. %% We don't need the result, but the handler has to send something.


start(Host, Opts) -> 
	crypto:start(),
	ssl:start(),
	gen_iq_handler:add_iq_handler(ejabberd_local, Host, <<?NS_APNS>>, ?MODULE, iq, no_queue),
	ejabberd_hooks:add(offline_message_hook, Host, ?MODULE, message, 49),
	ejabberd_hooks:add(muc_filter_message, Host, ?MODULE, muc_message, 49),
	?INFO_MSG("mod_apns Has started successfully!", []),
	ok.

stop(Host) -> ok.

mod_opt_type(address) -> fun iolist_to_binary/1; %binary_to_list?
mod_opt_type(port) -> fun(I) when is_integer(I) -> I end;
mod_opt_type(certfile) -> fun iolist_to_binary/1;
mod_opt_type(keyfile) -> fun iolist_to_binary/1;
mod_opt_type(password) -> fun iolist_to_binary/1;
mod_opt_type(_) ->
    [address, port, certfile, keyfile, password].
