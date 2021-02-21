-module(table_util).

-include("table.hrl").
-include("logger.hrl").

%%%===================================================================
%%% API
%%%===================================================================

-export([spawn_position/1, sort_seats/1, sort_key/2, sort_next_key/2]).
-export([increase_chips/2, reduce_chips/2]).
-export([increase_chips_by_seat/2]).
-export([init_cards/0, shuffle/1, shuffle/2, deal_nplayer_per_ncards/3]).
-export([get_number_of_deal_cards_by_game_type/1, get_speed_sec/1]).
-export([get_seat_id/1, get_seat/2, replace_seat/2, delete_seat/2, delete_seat_by_id/2]).
-export([get_seat_of_data/2, update_seat_of_data/2, get_exist_hand_of_data/1]).
-export([clear_seats_round_bet_of_data/1, update_current_pot_of_data/1]).
-export([delete_exist_seat_of_data/2, delete_action_seat_of_data/2]).
-export([delete_round_seat_of_data/2, update_round_top_bet_of_data/2]).
-export([increase_pot_of_data/3, reset_round_seat/1]).
-export([set_active_seat/2, set_next_active_seat/2, next_active_seat/1]).
-export([is_hand_ending/1]).
-export([merge_pot_record/1]).
-export([get_active_seat_id/1]).

%% -------------------------------------------------------------------
spawn_position(List) ->
    case length(List) of
        2 ->
            ['BTN', 'BB'];
        3 ->
			['BTN', 'SB', 'BB'];
        4 ->
			['BTN', 'SB', 'BB', 'UTG'];
        5 ->
			['BTN', 'SB', 'BB', 'UTG', 'CO'];
        6 ->
			['BTN', 'SB', 'BB', 'UTG', 'HJ', 'CO'];
        7 ->
			['BTN', 'SB', 'BB', 'UTG', 'LJ', 'HJ', 'CO'];
        8 ->
			['BTN', 'SB', 'BB', 'UTG', 'UTG1', 'LJ', 'HJ', 'CO'];
        9 ->
			['BTN', 'SB', 'BB', 'UTG', 'UTG1', 'UTG2', 'LJ', 'HJ', 'CO'];
        10 ->
			['BTN', 'SB', 'BB', 'UTG', 'UTG1', 'UTG2', 'UTG3', 'LJ', 'HJ', 'CO']
    end.

%% -------------------------------------------------------------------
sort_seats(Seats) ->
    lists:sort(fun(A, B) ->
        A#seat.id < B#seat.id
    end, Seats).

%% -------------------------------------------------------------------
sort_key(List, Key) ->
    {L, M} =  lists:foldl(fun(X, {Less, More}) ->
        case X < Key of
            true ->
                {[X | Less], More};                
            false ->
                {Less, [X | More]}
        end
    end, {[],[]}, List),
    lists:reverse(L ++ M).

sort_next_key(List, Key) ->
    {L, M} =  lists:foldl(fun(X, {Less, More}) ->
        case X > Key of
            true ->
                {Less, [X | More]};
            false ->
                {[X | Less], More}
        end
    end, {[],[]}, List),
    lists:reverse(L ++ M).

% ------------------------------------------------------------------------------
increase_chips(Chips, Value) ->
	Chips + Value.

reduce_chips(Chips, Value) ->
    case check_chips(Chips, Value) of
        true ->
            {Chips - Value, Value};
        false ->
            {0, Chips}
    end.

check_chips(Chips, Value) ->
    Chips >= Value.

% ------------------------------------------------------------------------------
increase_chips_by_seat(Value, Seat) ->
    #seat{chips = Chips} = Seat,
	NewChips = increase_chips(Chips, Value),
    Seat#seat{chips = NewChips}.

% ------------------------------------------------------------------------------
init_cards() ->
    [
        "SA", "SK", "SQ", "SJ", "ST", "S9", "S8", "S7", "S6", "S5", "S4", "S3", "S2",
        "HA", "HK", "HQ", "HJ", "HT", "H9", "H8", "H7", "H6", "H5", "H4", "H3", "H2",
        "DA", "DK", "DQ", "DJ", "DT", "D9", "D8", "D7", "D6", "D5", "D4", "D3", "D2",
        "CA", "CK", "CQ", "CJ", "CT", "C9", "C8", "C7", "C6", "C5", "C4", "C3", "C2"
    ].

shuffle(List) ->
    {R, _} = rand1:split(length(List), List),
    R.

shuffle(List, 0) ->
    List;
shuffle(List, Times) ->
    R = shuffle(List),
    shuffle(R, Times-1).

% ------------------------------------------------------------------------------
%% 牌发给几个人每人几张
%% List -> 洗好的牌, Nop -> 几个人, NoC -> 每人几张
deal_nplayer_per_ncards(List, NoP, NoC) ->
    {PC, Remain} =
    lists:mapfoldl(fun(_, Acc) ->
        [H|T] = Acc,
        {[H], T}
    end, List, lists:seq(1, NoP)),
    deal_nplayer_per_ncards_1(PC, Remain, NoC-1).

deal_nplayer_per_ncards_1(PlayerCards, RemainCards, 0) ->
    {PlayerCards, RemainCards};
deal_nplayer_per_ncards_1(PlayerCards, RemainCards, N) ->
    {PC, Remain} =
    lists:mapfoldl(fun(X, Acc) ->
        [H|T] = Acc,
        {[H|X], T}
    end, RemainCards, PlayerCards),
    deal_nplayer_per_ncards_1(PC, Remain, N-1).

% ------------------------------------------------------------------------------

get_number_of_deal_cards_by_game_type(Type) ->
    case Type of
        'HOLDEM' ->
            2;
        'OMAHA' ->
            4;
        _ ->
            0
    end.

% ------------------------------------------------------------------------------

get_speed_sec(Speed) ->
    case Speed of
        'SLOW' ->
            30;
        'NORMAL' ->
            20;
        'FAST' ->
            10;
        'RAPIDLY' ->
            5;
        'GOD' ->
            1
    end.

% ------------------------------------------------------------------------------

get_seat_id(Seat) ->
    #seat{id = ID} = Seat,
    ID.

get_seat(ID, Seats) ->
    lists:keyfind(ID, #seat.id, Seats).

replace_seat(Seat, Seats) ->
    #seat{id = ID} = Seat,
    lists:keyreplace(ID, #seat.id, Seats, Seat).

delete_seat(Seat, Seats) ->
    #seat{id = ID} = Seat,
    lists:keydelete(ID, #seat.id, Seats).

delete_seat_by_id(SeatID, Seats) ->
    lists:keydelete(SeatID, #seat.id, Seats).

% ------------------------------------------------------------------------------

get_seat_of_data(SeatID, Data) ->
    #table_data{seats = Seats} = Data,
    get_seat(SeatID, Seats).

update_seat_of_data(Seat, Data) ->
    #table_data{seats = Seats} = Data,
    NewSeats = replace_seat(Seat, Seats),
    Data#table_data{seats = NewSeats}.

get_exist_hand_of_data(Data) ->
    #table_data{seats = Seats, exist_seats = ExistSeats} = Data,
    lists:map(fun(X) ->
        Seat = get_seat(X, Seats),
        #seat{id = ID, hands = Hands} = Seat,
        {ID, Hands}
    end, ExistSeats).

clear_seats_round_bet_of_data(Data) ->
    #table_data{seats = Seats} = Data,    
    NewSeats = lists:map(fun(X) ->
        X#seat{round_bet = 0}
    end, Seats),
    Data#table_data{seats = NewSeats, round_top_bet = 0}.

update_current_pot_of_data(Data) ->
    #table_data{
        total_pot = TotalPot,
        total_side_pot = TotalSidePot
    } = Data,
    CurrentPot = TotalPot - TotalSidePot,
    Data#table_data{current_pot = CurrentPot}.

increase_pot_of_data(Data, Value, RoundTopBet) ->
    #table_data{
        total_pot = TotalPot,
        round_top_bet = OldRoundTopBet,
        min_raise = MinRaise
    } = Data,    

    NewRoundTopBet = case RoundTopBet > OldRoundTopBet of
        true ->
            RoundTopBet;
        false ->
            OldRoundTopBet
    end,

    NewMinRaise =
    case NewRoundTopBet - OldRoundTopBet of
        C when C > MinRaise ->
            C;
        _ ->
            MinRaise
    end,

    NewTotalBet = TotalPot + Value,

    Data#table_data{
        total_pot = NewTotalBet,
        round_top_bet = NewRoundTopBet,
        min_raise = NewMinRaise
    }.

reset_round_seat(Data) ->
    #table_data{action_seats = ActionSeats} = Data,
    Data#table_data{round_seats = ActionSeats}.


% ------------------------------------------------------------------------------

delete_exist_seat_of_data(SeatID, Data) ->
    ?DEBUG("delete here: ~p~n", [SeatID]),
    #table_data{exist_seats = ExistSeats, action_seats = ActionSeats, round_seats = RoundSeats} = Data,
    NewExistSeats = lists:delete(SeatID, ExistSeats),
    NewActionSeats = lists:delete(SeatID, ActionSeats),
    NewRoundSeats = lists:delete(SeatID, RoundSeats),
    table_dict:delete_seat(SeatID),
    Data#table_data{exist_seats = NewExistSeats, action_seats = NewActionSeats, round_seats = NewRoundSeats}.

delete_action_seat_of_data(SeatID, Data) ->
    ?DEBUG("delete here: ~p~n", [SeatID]),
    #table_data{action_seats = ActionSeats, round_seats = RoundSeats} = Data,
    NewActionSeats = lists:delete(SeatID, ActionSeats),
    NewRoundSeats = lists:delete(SeatID, RoundSeats),
    table_dict:delete_seat(SeatID),
    Data#table_data{action_seats = NewActionSeats, round_seats = NewRoundSeats}.

delete_round_seat_of_data(SeatID, Data) ->
    #table_data{round_seats = RoundSeats} = Data,
    NewRoundSeats = lists:delete(SeatID, RoundSeats),
    Data#table_data{round_seats = NewRoundSeats}.

update_round_top_bet_of_data(Value, Data) ->
    #table_data{round_top_bet = RoundTopBet} = Data,
    case Value > RoundTopBet of
        true ->
            Data#table_data{round_top_bet = Value};
        false ->
            Data
    end.
        

% ------------------------------------------------------------------------------

set_active_seat(SeatID, Data) ->
    case SeatID of
        undefined ->
            actor_action:sg();
        _ ->
            ok
    end,
    #table_data{speed = Speed} = Data,
    Sec = table_util:get_speed_sec(Speed),
    NewData = Data#table_data{active_seat = {SeatID, time:now()}},
    table_msg:broadcast_active_seat(SeatID),
%%    {{timeout, Sec*1000}, NewData}.


    Seat = get_seat_of_data(SeatID, NewData),

    case Seat of
        false ->
            ?DEBUG("data: ~p~n", [NewData]);
        _ ->
            ok
    end,

    #seat{is_robot = IsRobot} = Seat,
    case IsRobot of
        true ->
            {{timeout, 100}, NewData};
        false ->
            {{timeout, Sec*1000}, NewData}
    end.



set_next_active_seat(NextSeatID, Data) ->
    set_active_seat(NextSeatID, Data).

next_active_seat(Data) ->
    #table_data{active_seat = ActiveSeat} = Data,
    {SeatID, _} = ActiveSeat,
    set_next_active_seat(SeatID, Data).

% ------------------------------------------------------------------------------
is_hand_ending(Data) ->
    #table_data{exist_seats = ExistSeats} = Data,
    length(ExistSeats) =:= 1.

%% -------------------------------------------------------------------
%% merge_pot_record
%% -------------------------------------------------------------------
merge_pot_record(List) ->
    S = lists:sort(fun(A, B) ->
        {I1, _, _} = A,
        {I2, _, _} = B,
        I1 < I2
                   end, List),
    [H | T] = S,

    {Result, _} =
        lists:foldl(fun(X, {Acc, AccM}) ->
            {I1, V1, P1} = X,
            {I2, V2, P2} = AccM,
            case I1 of
                I2 ->
                    V = V1 + V2,
                    P = P1 ++ P2,
                    NewAcc = lists:keyreplace(I1, 1, Acc, {I1, V, lists:sort(P)}),
                    {NewAcc, X};
                _ ->
                    {[X|Acc], X}
            end
                    end, {[H], H}, T),
    lists:reverse(Result).

get_active_seat_id(Data) ->
    #table_data{active_seat = ActiveSeat} = Data,
    {SeatID, _} = ActiveSeat,
    SeatID.

%%%===================================================================
%%% Internal
%%%===================================================================

