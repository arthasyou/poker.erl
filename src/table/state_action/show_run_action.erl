%%%-------------------------------------------------------------------
%%% @author luobin
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. Feb 2021 12:48 PM
%%%-------------------------------------------------------------------
-module(show_run_action).
-author("luobin").

-include("table.hrl").
-include("logger.hrl").

%% API
-export([start/1]).
-export([deal_flop/1, deal_turn/1, deal_river/1]).
-export([ending/1]).

start(Data) ->
    table_msg:broadcast_show_hand(Data),
    #table_data{community_cards = CommunityCards} = Data,
    Action = case length(CommunityCards) of
                 0 ->
                     deal_flop;
                 3 ->
                     deal_turn;
                 4 ->
                     deal_river
             end,
    Action.

deal_flop(Data) ->
    Data1 = table_deal_card:flop(Data),
    Action = {{timeout, show_run}, 1000, deal_turn},
    {action, Action, Data1}.

deal_turn(Data) ->
    Data1 = table_deal_card:turn_or_river(Data),
    Action = {{timeout, show_run}, 1000, deal_river},
    {action, Action, Data1}.

deal_river(Data) ->
    Data1 = table_deal_card:turn_or_river(Data),
    Action = {{timeout, show_run}, 1000, ending},
    {action, Action, Data1}.

ending(Data) ->
    #table_data{community_cards = CommunityCards} = Data,
    Hands = table_util:get_exist_hand_of_data(Data),
    Results = alg_poker:get_hands_results(Hands, CommunityCards),
    Data1 = common_action:win_the_pot(Results, Data),
    table_msg:broadcast_win(Data1),
    Data2 = common_action:init(Data1),
    {{next_state, waiting}, Data2}.