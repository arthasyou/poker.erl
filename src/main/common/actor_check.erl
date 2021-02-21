%%%-------------------------------------------------------------------
%%% @author luobin
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. Feb 2021 4:45 PM
%%%-------------------------------------------------------------------
-module(actor_check).
-author("luobin").

%% API
-export([is_in_the_table/1]).

is_in_the_table(TableID) ->
	All = main_dict:get_all_table(),
	lists:member(TableID, All).
