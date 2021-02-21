-module(login_cmd).

-include("logger.hrl").
-include("all_pb.hrl").

%% ===========================API============================
-export([handle/2]).

handle(1001, Data) ->
	#m_1001_tos{id = ID} = Data,
	login_callback:login(ID);

handle(Cmd, _Data) ->
	?ERROR_MSG("unknow cmd: ~p~n", [Cmd]),
	noreply.


%% ===========================Internal============================

