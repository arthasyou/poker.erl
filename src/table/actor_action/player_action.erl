%%%-------------------------------------------------------------------
%%% @author luobin
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. Feb 2021 8:05 PM
%%%-------------------------------------------------------------------
-module(player_action).
-author("luobin").

-include("table.hrl").
-include("logger.hrl").

%% API
-export([sit_down/4, ready/3]).
-export([action/3]).

sit_down(_State, Data, PlayerID, SeatID) ->
	case table_check:can_sit_down(PlayerID, SeatID, Data) of
		true ->
			Data1 = actor_action:sit_down(PlayerID, SeatID, Data),
			table_dict:put_player_seat_id(PlayerID, SeatID),
			{ok, {ok, Data1}};
		Error ->
			Error
	end.

ready(_State, Data, PlayerID) ->
	case table_check:can_ready(PlayerID, Data) of
		{true, SeatID} ->
			Data1 = actor_action:ready(SeatID, Data),
			{ok, {ok, Data1}};
		Error ->
			Error
	end.

%% -------------------------------------------------------------------
%% player action
%% -------------------------------------------------------------------
action(PlayerID, Action, Data) ->
	case table_check:can_action(PlayerID, Data) of
		{true, SeatID} ->
			case SeatID of
				undefined ->
					?DEBUG("unknow seat: ~p~n", [Data]);
				_ ->
					ok
			end,
			StateCallback = actor_action:action(SeatID, Action, Data),
			{ok, StateCallback};
		Error ->
			Error
	end.


