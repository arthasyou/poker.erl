%% =========================================================
%% 使用说明
%%  
%% =========================================================
-module(packet).

-include("logger.hrl").


%% ===========================API============================
-export([unpack/1, unpack_web/2]).
-export([pack_data/1, pack_error/1, pack_web/1]).

unpack(Bin) ->
	% ?DEBUG("Bin:~p~n", [Bin]),
	<<Cmd:16, DataBin/binary>> = Bin,
	% ?DEBUG("Cmd:~p~n", [Cmd]),
	Data = protobuf_packet:decode(Cmd, DataBin),
	% ?DEBUG("Data:~p~n", [Data]),
	{ok, {Cmd, Data}}.

unpack_web(Bin, Opcode) ->
	case Opcode of
		2 ->
			unpack(Bin);
		_ ->
			{ok, Bin}	
	end.

pack_data(Args) ->
	{Cmd, Data} = Args,
	DataBin = protobuf_packet:encode(Data),
	% ?DEBUG("Data:~p~n", [DataBin]),
	<<0:16, Cmd:16, DataBin/binary>>.

pack_error(Args) ->
	{Cmd, ErrorCode} = Args,
	<<ErrorCode:16, Cmd:16>>. 

%% you can pack web with opcode default is 2
pack_web(Bin) ->
	web_packet:encode(Bin).
	% web_packet:encode(Bin, 1).
%% ===========================Internal============================

