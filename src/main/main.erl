-module(main).

%% ===========================API============================
-export([start/0]).
-export([start_child/2, get_pid/1, get_all/0, get_all_id/0]).
-export([call/4, cast/4, delay_msg/5]).
-export([response/3, broadcast/3, broadcast_all/2]).

start() ->
	counter:init(main_id).

start_child(ID, SockPID) ->
	case get_pid(ID) of
		null ->
			main_sup:start_child([ID, SockPID]);
		{ok, _} ->
			{error, "already start the child"}
	end.

get_pid(ID) ->
	main_mgr:get_pid(ID).

get_all() ->
	main_mgr:get_all().

get_all_id() ->
	main_mgr:get_all_id().

call(ID, Module, Function, Args) ->
	case check_id(ID) of
		{ok, PID} ->
			case self() of
				PID ->
					{error, "can't call self"};
				_ ->
					Msg = mfa_msg(Module, Function, Args),
					gen_server:call(PID, Msg)
			end;
		Error ->
			Error
	end.

cast(ID, Module, Function, Args) ->
	case check_id(ID) of
		{ok, PID} ->
			Msg = mfa_msg(Module, Function, Args),
			gen_server:cast(PID, Msg);
		Error ->
			Error
	end.
	
delay_msg(ID, Time, Module, Function, Args) ->
	case check_id(ID) of
		{ok, PID} ->
			Msg = mfa_msg(Module, Function, Args),
			erlang:send_after(Time, PID, Msg);
		Error ->
			Error
	end.

%% response to single client with socket 
response(ID, Cmd, Data) ->
	case check_id(ID) of
		{ok, PID} ->
			Msg = pack_msg(Cmd, Data),
			gen_server:cast(PID, Msg);
		Error ->
			Error
	end.

%% broadcast to spec clients with socket 
broadcast(IDS, Cmd, Data) ->
	Bin = packet:pack_data({Cmd, Data}),
	Msg = {response, Bin},
	broadcast(IDS, Msg).

%% broadcast to all clients with socket 
broadcast_all(Cmd, Data) ->
	List = get_all_id(),
	broadcast(List, Cmd, Data).

%% ===========================Internal============================

check_id(ID) ->
	case get_pid(ID) of
		null ->
			{error, "the process not found"};
		Reply ->
			Reply
	end.

mfa_msg(M, F, A) ->
	{mfa, {M, F, A}}.

pack_msg(Cmd, Data) ->
	Bin = packet:pack_data({Cmd, Data}),
	{response, Bin}.

do_response(ID, Msg) ->
	case check_id(ID) of
		{ok, PID} ->
			gen_server:cast(PID, Msg);
		_ ->
			ignore
	end.

broadcast(List, Msg) ->
	lists:foreach(fun(X) ->
		do_response(X, Msg)
	end, List).




