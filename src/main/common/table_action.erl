%%%-------------------------------------------------------------------
%%% @author luobin
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. Feb 2021 4:42 PM
%%%-------------------------------------------------------------------
-module(table_action).
-author("luobin").

%% API
-export([sit_down/3, ready/2]).
-export([fold/2, check_call/2, bet_raise/3]).

sit_down(HeroID, TableID, SeatID) ->
	case actor_check:is_in_the_table(TableID) of
		true ->
			table:sit_down(HeroID, TableID, SeatID);
		false ->
			false
	end.

ready(HeroID, TableID) ->
	case actor_check:is_in_the_table(TableID) of
		true ->
			table:ready(HeroID, TableID);
		false ->
			false
	end.

fold(HeroID, TableID) ->
	case actor_check:is_in_the_table(TableID) of
		true ->
			table:fold(HeroID, TableID);
		false ->
			false
	end.

check_call(HeroID, TableID) ->
	case actor_check:is_in_the_table(TableID) of
		true ->
			table:check_call(HeroID, TableID);
		false ->
			false
	end.

bet_raise(HeroID, TableID, Chips) ->
	case actor_check:is_in_the_table(TableID) of
		true ->
			table:bet_raise(HeroID, TableID, Chips);
		false ->
			false
	end.



