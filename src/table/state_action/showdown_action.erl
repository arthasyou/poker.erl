%%%-------------------------------------------------------------------
%%% @author luobin
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Feb 2021 12:59 PM
%%%-------------------------------------------------------------------
-module(showdown_action).
-author("luobin").

-include("table.hrl").
-include("logger.hrl").

%% API
-export([start/1]).

start(Data) ->
    table_msg:broadcast_show_hand(Data),
    Hands = table_util:get_exist_hand_of_data(Data),
    #table_data{community_cards = CommunityCards} = Data,
    Results = alg_poker:get_hands_results(Hands, CommunityCards),
    Data1 = common_action:win_the_pot(Results, Data),
    table_msg:broadcast_win(Data1),
    Data2 = common_action:init(Data1),
    Data2.