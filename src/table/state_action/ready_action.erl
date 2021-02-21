%%%-------------------------------------------------------------------
%%% @author luobin
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Feb 2021 4:23 PM
%%%-------------------------------------------------------------------
-module(ready_action).
-author("luobin").

-include("table.hrl").
-include("logger.hrl").

%% API
-export([start/1]).

start(Data) ->
    Data1 = assign_position(Data),
    Data2 = table_deal_card:preflop(Data1),
    table_msg:broadcast_ready_state(Data2),
    {enter, Data2}.

assign_position(Data) ->
    #table_data{
        seats = Seats,
        exist_seats = ExS,
        last_btn_seat = LBS
    } = Data,
    ExistSeats = table_util:sort_next_key(ExS, LBS),
    Positions = table_util:spawn_position(ExistSeats),
    KeyPos = lists:zip(ExistSeats, Positions),

    NewSeats = lists:foldl(fun(X, S) ->
        {ID, Pos} = X,
        Seat = table_util:get_seat(ID, S),
        NewSeat = Seat#seat{position = Pos, state = 'ACTIVE'},
        table_util:replace_seat(NewSeat, S)
    end, Seats, KeyPos),

    [Head | Tail] = ExistSeats,
    NewExistSeats = Tail ++ [Head], %% move BTN to the last
    spawn_seat_chain(ExistSeats, Head),

    Data#table_data{
        seats = NewSeats,
        exist_seats = NewExistSeats,
        action_seats = NewExistSeats,
        last_btn_seat = Head
    }.

spawn_seat_chain([Last], Head) ->
    table_dict:put_seat_chain(Last, Head);
spawn_seat_chain([H|T], Head) ->
    [H2|_] = T,
    table_dict:put_seat_chain(H, H2),
    spawn_seat_chain(T, Head).