%%%-------------------------------------------------------------------
%%% @author luobin
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. Feb 2021 8:09 PM
%%%-------------------------------------------------------------------
-module(table_check).
-author("luobin").

-include("table.hrl").
-include("error_code.hrl").

%% API

-export([is_empty_seat/2, is_player_at_seat/1]).

-export([can_sit_down/3, can_ready/2, can_action/2]).

%% -------------------------------------------------------------------
%% can_sit_down
%% -------------------------------------------------------------------
can_sit_down(PlayerID, SeatID, Data) ->
	case is_player_at_seat(PlayerID) of
		false ->
			is_empty_seat(SeatID, Data);
		_ ->
			{error, ?ALREADY_HAD_SEAT}
	end.

%% -------------------------------------------------------------------
%% can_ready
%% -------------------------------------------------------------------
can_ready(PlayerID, Data) ->
	case is_player_at_seat(PlayerID) of
		{true, SeatID} ->
			Seat = table_util:get_seat_of_data(SeatID, Data),
			#seat{state = State} = Seat,
			case State of
				'SIT_OUT' ->
					{true, SeatID};
				_ ->
					{error, ?ILLEGAL_STATE}
			end;
		_ ->
			{error, ?NO_SEAT}
	end.

%% -------------------------------------------------------------------
%% can_action
%% -------------------------------------------------------------------
can_action(PlayerID, Data) ->
	case is_player_at_seat(PlayerID) of
		{true, SeatID} ->
			case table_util:get_active_seat_id(Data) of
				SeatID ->
					{true, SeatID};
				_ ->
					{error, ?ILLEGAL_ACTION}
			end;
		_ ->
			{error, ?NO_SEAT}
	end.


%% -------------------------------------------------------------------
%% check
%% -------------------------------------------------------------------

is_empty_seat(SeatID, Data) ->
	#table_data{seats = Seats, total_seats = TotalSeat} = Data,
	case SeatID > 0 andalso SeatID =< TotalSeat of
		true ->
			case lists:keyfind(SeatID, #seat.id, Seats) of
				false ->
					true;
				_ ->
					{error, ?SEAT_NOT_EMPTY}
			end;
		false ->
			{error, ?ILLEGAL_SEAT}
	end.

is_player_at_seat(PlayerID) ->
	case table_dict:get_player_seat_id(PlayerID) of
		undefined ->
			false;
		SeatID ->
			{true, SeatID}
	end.