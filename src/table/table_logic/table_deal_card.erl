%%%-------------------------------------------------------------------
%%% @author luobin
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Feb 2021 4:32 PM
%%%-------------------------------------------------------------------
-module(table_deal_card).
-author("luobin").

-include("table.hrl").

%% API
-export([preflop/1, flop/1, turn_or_river/1, community/1]).

%% -------------------------------------------------------------------
%% preflop
%% -------------------------------------------------------------------
preflop(Data) ->
    RandCards = shuffle(),
    #table_data{
        game_type = GameType,
        exist_seats = ExistSeats
    } = Data,
    N = table_util:get_number_of_deal_cards_by_game_type(GameType),
    {ActorCards, RemainCards} = table_util:deal_nplayer_per_ncards(RandCards, length(ExistSeats), N),
    [_, C1, C2, C3, _, C4, _, C5| _] = RemainCards,
    assign_cards(Data, ActorCards, [C1, C2, C3, C4, C5]).

shuffle() ->
    Cards = table_util:init_cards(),
    table_util:shuffle(Cards, 5).

assign_cards(Data, ActorCards, RemainCards) ->
    #table_data{seats = Seats, exist_seats = ExistSeats} = Data,
    {_, NewSeats} =
        lists:foldl(fun(X, {A, S}) ->
            [H|T] = A,
            Seat = table_util:get_seat(X, S),
            NewSeat = Seat#seat{hands = H},
            NewS = table_util:replace_seat(NewSeat, S),
            {T, NewS}
                    end, {ActorCards, Seats}, ExistSeats),
    Data#table_data{remain_cards = RemainCards, seats = NewSeats}.

%% -------------------------------------------------------------------
%% flop
%% -------------------------------------------------------------------
flop(Data) ->
    #table_data{remain_cards = RemainCards} = Data,
    {FlopCards, RestCards} = lists:split(3, RemainCards),
    table_msg:broadcast_flop(FlopCards),
    Data#table_data{community_cards = FlopCards, remain_cards = RestCards}.

%% -------------------------------------------------------------------
%% turn_or_river
%% -------------------------------------------------------------------
turn_or_river(Data) ->
    #table_data{community_cards = CommunityCards, remain_cards = RemainCards} = Data,
    [Head | T] = RemainCards,
    CC = lists:reverse([Head | lists:reverse(CommunityCards)]),
    table_msg:broadcast_turn(Head),
    Data#table_data{community_cards = CC, remain_cards = T}.

%% -------------------------------------------------------------------
%% community
%% -------------------------------------------------------------------
community(Data) ->
    #table_data{
        community_cards = CommunityCards,
        remain_cards = RemainCards
    } = Data,
    Len = length(CommunityCards),
    case Len of
        5 ->
            Data;
        _ ->
            % Data#table_data{community_cards = ["SA", "SK", "SQ", "SJ", "ST"], remain_cards = []}
            NCC = CommunityCards ++ RemainCards,
            Data#table_data{community_cards = NCC, remain_cards = []}
    end.