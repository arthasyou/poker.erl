-module(main_cmd).

%% ===========================API============================
-export([handle/3]).

handle(Cmd, Data, ID) ->
	case get_mod(Cmd) of
		undefined ->
			noreply;
		Mod ->
			Mod:handle(Cmd, Data, ID)
	end.

%% ===========================Internal============================

get_mod(Cmd) ->
	case Cmd div 100 of
		20 ->
			holdem_cmd;
		_ ->
			undefined
	end.
