%%%-------------------------------------------------------------------
%%% @author luobin
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Feb 2021 12:44 PM
%%%-------------------------------------------------------------------
-module(preflop_action).
-author("luobin").

-include("table.hrl").

%% API
-export([start/1]).

start(Data) ->
    #table_data{exist_seats = ExistSeats, action_seats = ActionSeats} = Data,

    NoActionSeat =
        case length(ExistSeats) of
            2 ->
                lists:nth(1, ExistSeats);
            _ ->
                lists:nth(2, ExistSeats)
        end,

    AS = lists:delete(NoActionSeat, ActionSeats),

    case length(AS) of
        0 ->
            common_action:showdown_run(Data);
        1 ->
            ActiveSeat = lists:nth(1, AS),
            table_util:set_active_seat(ActiveSeat, Data);
        _ ->
            [H | T] = AS,
            ActiveSeat =
                case table_dict:get_next_seat(H) of
                    NoActionSeat ->
                        lists:nth(1, T);
                    _ ->
                        H
                end,
            table_util:set_active_seat(ActiveSeat, Data)
    end.
