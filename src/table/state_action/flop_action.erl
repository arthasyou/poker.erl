%%%-------------------------------------------------------------------
%%% @author luobin
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Feb 2021 12:42 PM
%%%-------------------------------------------------------------------
-module(flop_action).
-author("luobin").

-include("table.hrl").

%% API
-export([start/1]).

start(Data) ->
    Data1 = table_deal_card:flop(Data),
    #table_data{round_seats = RoundSeat} = Data1,
    [SeatID | _] = RoundSeat,
    table_util:set_active_seat(SeatID, Data1).