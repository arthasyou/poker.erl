%%%-------------------------------------------------------------------
%%% @author luobin
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Feb 2021 4:23 PM
%%%-------------------------------------------------------------------
-module(ante_action).
-author("luobin").

%% API
-export([start/1]).

start(Data) ->
    Data1 = table_pot:ante_pot(Data),
    Data2 = table_pot:round_side_pot(Data1),
    Data3 = table_util:update_current_pot_of_data(Data2),
    Data4 = table_util:clear_seats_round_bet_of_data(Data3),
    table_msg:broadcast_put_in_blind(Data4),
    {enter, Data4}.
