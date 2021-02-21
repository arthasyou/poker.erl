%% @author YouShuxiang
%% @doc @todo Add description to router.


-module(router).

-include("tcp.hrl").
-include("all_pb.hrl").
-include("logger.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([rout_msg/2]).

rout_msg(Args, State) ->
	{Cmd, Data} = Args,
	MainPID = State#tcp_state.main_pid,
	case MainPID of
		undefined ->
			login:cmd(Cmd, Data);
		_ ->
			Msg = {cmd, {Cmd, Data}},
			gen_server:cast(MainPID, Msg)
	end.



