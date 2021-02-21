%%%-------------------------------------------------------------------
%%% @author luobin
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Feb 2021 4:09 PM
%%%-------------------------------------------------------------------
-module(waiting_action).
-author("luobin").

-include("table.hrl").

%% API
-export([start/1]).

start(Data) ->
    case check_seats(Data) of
        {true, NewData} ->
            {ready, NewData};
        {false, NewData} ->
            {waiting, NewData}
    end.

check_seats(Data) ->
    Data1 = sit_out_empty_chips(Data),
    #table_data{seats = Seats} = Data1,
    ExistSeats = lists:foldl(fun(X, Acc) ->
        #seat{
            id = ID,
            state = State
        } = X,
        case State of
            'READY' ->
                [ID | Acc];
            _ ->
                Acc
        end
    end, [], Seats),
    case length(ExistSeats) >= 2 of
        true ->
            NewSeats = table_util:sort_seats(Seats),
            {true, Data1#table_data{
                seats = NewSeats,
                exist_seats = lists:sort(ExistSeats)
            }};
        false ->
            {false, Data1}
    end.

sit_out_empty_chips(Data) ->
    #table_data{seats = Seats} = Data,
    NewSeats =
        lists:map(fun(X) ->
            #seat{
                chips = Chips
            } = X,
            case Chips of
                0 ->
                    X#seat{state = 'SIT_OUT'};
                _ ->
                    X
            end
        end, Seats),
    Data#table_data{seats = NewSeats}.