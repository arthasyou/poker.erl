%%%-------------------------------------------------------------------
%%% @author luobin
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Feb 2021 12:50 PM
%%%-------------------------------------------------------------------
-module(common_action).
-author("luobin").

-include("table.hrl").
-include("logger.hrl").

%% API
-export([end_of_round/1, ending/1]).
-export([showdown_run/1]).
-export([win_the_pot/2, one_pot/2]).
-export([init/1]).

%% -------------------------------------------------------------------
%% Next Round
%% -------------------------------------------------------------------

end_of_round(Data) ->
    Data1 = table_pot:round_side_pot(Data),
    Data2 = table_util:update_current_pot_of_data(Data1),
    Data3 = table_util:clear_seats_round_bet_of_data(Data2),
    table_msg:broadcast_end_of_round(Data3),
    case check_showdown(Data3) of
        true ->
            common_action:showdown_run(Data3);
        false ->
            {next_round, Data3}
    end.

check_showdown(Data) ->
    #table_data{
        action_seats = ActionSeats,
        community_cards = ConmunityCards
    } = Data,
    case length(ActionSeats) =< 1 of
        true ->
            case length(ConmunityCards) of
                5 ->
                    false;
                _ ->
                    true
            end;
        false ->
            false
    end.

%% -------------------------------------------------------------------

ending(Data) ->
    #table_data{exist_seats = [SeatID]} = Data,
    Data1 = common_action:one_pot(SeatID, Data),
    Data2 = common_action:init(Data1),
    {{next_state, waiting}, Data2}.

showdown_run(Data) ->
    {{next_state, show_run}, Data}.

%% -------------------------------------------------------------------
%% one_pot
%% -------------------------------------------------------------------
one_pot(SeatID, Data) ->
    #table_data{total_pot = TotalPot} = Data,
    Seat = table_util:get_seat_of_data(SeatID, Data),
    NewSeat = table_util:increase_chips_by_seat(TotalPot, Seat),
    table_util:update_seat_of_data(NewSeat, Data).

%% -------------------------------------------------------------------
%% win_the_pot
%% -------------------------------------------------------------------
win_the_pot(Results, Data) ->
    Winners = alg_poker:get_winner(Results),
    Len = length(Winners),
    {NewData, Flag} =
        case Len of
            1 ->
                [SeatID] = Winners,
                single_pot(SeatID, Data);
            _ ->
                split_pot(Winners, Data)
        end,
    case Flag of
        done ->
            NewData;
        empty ->
            take_current_pot(NewData);
        continued ->
            NewResults = get_continued_results(Results, NewData),
            win_the_pot(NewResults, NewData)
    end.

take_current_pot(Data) ->
    #table_data{exist_seats = [SeatID]} = Data,
    {Data1, _} = single_pot(SeatID, Data),
    Data1.

get_continued_results(Results, Data) ->
    #table_data{exist_seats = ExistSeats} = Data,
    lists:foldl(fun(X, Acc) ->
        [lists:keyfind(X, 1, Results) | Acc]
                end, [], ExistSeats).

win_seat(SeatID, Value, Data) ->
    Seat = table_util:get_seat_of_data(SeatID, Data),
    NewSeat = table_util:increase_chips_by_seat(Value, Seat),
    table_util:update_seat_of_data(NewSeat, Data).

win_seats(List, Data) ->
    lists:foldl(fun(X, Acc) ->
        {SeatID, Value} = X,
        win_seat(SeatID, Value, Acc)
                end, Data, List).

win_seats(List, Value, Data) ->
    lists:foldl(fun(X, Acc) ->
        win_seat(X, Value, Acc)
                end, Data, List).

single_pot(SeatID, Data) ->
    {WinValue, SidePot, Data1, Flag} = table_pot:get_seat_pot(SeatID, Data),
    side_pot_record(SidePot, [{SeatID, WinValue}]),
    Data2 = win_seat(SeatID, WinValue, Data1),
    {Data2, Flag}.

split_pot(Winners, Data) ->
    SeatID = table_pot:get_multi_pot_seat_id(Winners),
    {SumValue, SidePot, Data1, Flag} = table_pot:get_seat_pot(SeatID, Data),
    Len = length(Winners),
    AvgValue = SumValue div Len,
    N = SumValue rem Len,
    Data2 = assign_split_value(Winners, AvgValue, N, SidePot, Data1),
    {Data2, Flag}.

assign_split_value(Winners, Value, N, SidePot, Data) ->
    case N of
        0 ->
            VG = lists:map(fun(X) ->
                {X, Value}
            end, Winners),
            side_pot_record(SidePot, VG),
            win_seats(Winners, Value, Data);
        _ ->
            {BigGroup, SmallGroup} = rand1:split(N, Winners),
            TempGroup =
                lists:foldl(fun(X, Acc) ->
                    [{X, Value+1} | Acc]
                            end, [], BigGroup),

            Group =
                lists:foldl(fun(X, Acc) ->
                    [{X, Value} | Acc]
                            end, TempGroup, SmallGroup),
            side_pot_record(SidePot, Group),
            win_seats(Group, Data)
    end.


side_pot_record(SidePot, Actors) ->
    SIds = lists:map(fun({X, _}) ->
            X
    end, SidePot),
    R = lists:map(fun(X) ->
        {ActorID, V} = X,
        {ActorID, V, SIds}
    end, Actors),
    table_dict:put_win_pot_record(R),
    ok.

%% -------------------------------------------------------------------
%% hand end init data
%% -------------------------------------------------------------------

init(Data) ->
    table_dict:clear_side_pot(),
    table_dict:clear_win_pot_record(),
    Data1 = init_seat(Data),
    #table_data{bb = BB} = Data1,
    Data1#table_data{
        community_cards = [],
        total_pot = 0,
        current_pot = 0,
        side_pot = [],
        total_side_pot = 0,
        next_side_pot_id = 1,
        round_top_bet = BB,
        min_raise = BB,
        exist_seats = [],
        action_seats = [],
        round_start_seats = [],
        round_seats = [],
        active_seat = {0, 0},
        remain_cards = []
    }.

init_seat(Data) ->
    #table_data{seats = Seats} = Data,
    NewSeats = lists:map(fun(X) ->
        X#seat{
            state = set_ready_state(X#seat.state),
            position = 'NONE',
            round_bet = 0,
            total_bet = 0,
            hands = []
        }
                         end, Seats),
    Data#table_data{seats = NewSeats}.

set_ready_state(State) ->
    case State of
        S when S =:= 'SIT_OUT' orelse S =:= 'READY' ->
            State;
        _ ->
            'READY'
    end.


