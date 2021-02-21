-module(main_internal).

-include("logger.hrl").

%%%===================================================================
%%% API
%%%===================================================================
-export([init/1, terminate/1]).
-export([send_data/3, send_error/3]).

init(ID) ->
	%% TODO: Write your code here
	ok.

terminate(ID) ->
	leave_table(ID),
	ok.

%% response data to client
send_data(SockPID, Cmd, Data) ->
	Bin = packet:pack_data({Cmd, Data}),
	tcp_response:send(SockPID, Bin).

%% response error code to client
send_error(SockPID, Cmd, ErrorCode) ->
	Bin = packet:pack_error({Cmd, ErrorCode}),
	tcp_response:send(SockPID, Bin).


%%%===================================================================
%%% Internal
%%%===================================================================

leave_table(ID) ->
	List = main_dict:get_all_table(),
	lists:foreach(fun(X) ->
		table:leave_table(ID, X)
	end, List).



