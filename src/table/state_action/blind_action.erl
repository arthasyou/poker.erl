%%%-------------------------------------------------------------------
%%% @author luobin
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Feb 2021 4:27 PM
%%%-------------------------------------------------------------------
-module(blind_action).
-author("luobin").

-include("table.hrl").

%% API
-export([start/1]).

start(Data) ->
    Data1 = put_in_blind(Data),
    table_msg:broadcast_put_in_blind(Data1),
    {enter, Data1}.

put_in_blind(Data) ->
    Data1 = set_bb_as_top_bet(Data),
    table_pot:blind_pot(Data1).

set_bb_as_top_bet(Data) ->
    #table_data{bb = BB} = Data,
    Data#table_data{round_top_bet = BB, min_raise = BB}.