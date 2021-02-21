-module(login).

-include("tcp.hrl").

%% ===========================API============================
-export([cmd/2]).

cmd(Cmd, DataIn) ->
    case login_cmd:handle(Cmd, DataIn) of
        {ok, DataOut} ->
            response_data(Cmd, DataOut);
		{error, ErrorCode} ->
            response_error(Cmd, ErrorCode);
        noreply ->
            ok
    end.

%% ===========================Internal============================

response_data(Cmd, Data) ->
	Bin = packet:pack_data({Cmd, Data}),
	tcp_response:send(self(), Bin).

response_error(Cmd, ErrorCode) ->
	Bin = packet:pack_error({Cmd, ErrorCode}),
	tcp_response:send(self(), Bin).

