-module(actor_action).

-include("table.hrl").
-include("logger.hrl").
-include("error_code.hrl").

%%%===================================================================
%%% API
%%%===================================================================
-export([enter/3, leave/1, sit_down/3, ready/2]).
-export([timeout/1]).
-export([action/3]).

enter(State, Data, PlayerID) ->
    table_dict:player_enter(PlayerID),
    Msg = table_msg:get_enter_info(State, Data, PlayerID),
    {ok, Msg}.

leave(PlayerID) ->
    table_dict:player_leave(PlayerID),   
    ok.

sit_down(ActorID, SeatID, Data) ->
    Seat = #seat{id = SeatID, actor_id = ActorID, chips = 2000},
    #table_data{seats = Seats} = Data,
    NewSeats = [Seat | Seats],
    table_msg:broadcast_actor_sit_down(Seat, ActorID),
    Data#table_data{seats = NewSeats}.

ready(SeatID, Data) ->
    table_msg:broadcast_seat_state(SeatID, 'READY'),
    Seat = table_util:get_seat_of_data(SeatID, Data),
    NewSeat = Seat#seat{state = 'READY'},
    table_util:update_seat_of_data(NewSeat, Data).

%% -------------------------------------------------------------------

timeout(Data) ->
    Seat = get_active_seat(Data),
    Fun = fold_or_check(Seat, Data),
    Fun(Data).

%% -------------------------------------------------------------------

action(SeatID, Action, Data) ->
    NewAction = evaluate_action(SeatID, Action, Data),
    case get_action_fun(NewAction) of
        {Fun, Value} ->
            Fun(Value, Data);
        Fun ->
            Fun(Data)
    end.

%%%===================================================================
%%% Internal
%%%===================================================================

evaluate_action(SeatID, Action, Data) ->
    case Action of
        fold ->
            fold;
        check_call ->
            {CallValue, _, _} = get_call_value_and_raise_flag(SeatID, Data),
            case CallValue of
                0 ->
                    check;
                _ ->
                    call
            end;
        {_, Value} ->
            {_, MinRaiseValue, Flag} = get_call_value_and_raise_flag(SeatID, Data),
            case Flag of
                call ->
                    call;
                _ ->
                    case Value > MinRaiseValue of
                        true ->
                            {Flag, Value};
                        false ->
                            {Flag, MinRaiseValue}
                    end
            end
    end.

get_call_value_and_raise_flag(SeatID, Data) ->
    #table_data{
        round_top_bet = RoundTopBet,
        min_raise = MinRaise
    } = Data,
    Seat = table_util:get_seat_of_data(SeatID, Data),
    #seat{round_bet = ActorBet, chips = Chips} = Seat,
    CallValue = RoundTopBet - ActorBet,
    MinRaiseValue = CallValue + MinRaise,
    {Flag, NewMinRaiseValue} =
    case Chips of
       C when C > MinRaiseValue ->
           case CallValue of
               0 ->
                   {bet, MinRaiseValue};
               _ ->
                   {raise, CallValue+MinRaise}
%%                   case CallValue < MinRaise of
%%                       true ->
%%                           {call, 0};
%%                       _ ->
%%                           {raise, CallValue+MinRaise}
%%                   end
           end;
       C when C > CallValue ->
           {bet, C};
       _ ->
           {call, 0}
    end,
    {CallValue, NewMinRaiseValue, Flag}.




get_action_fun(Action) ->
    case Action of
        fold ->
            fun fold/1;
        check ->
            fun check/1;
        call ->
            fun call/1;
        {bet, Value} ->
            {fun bet/2, Value};
        {raise, Value} ->
            {fun raise/2, Value}
    end.


%% -------------------------------------------------------------------
%% fold
%% -------------------------------------------------------------------
fold(Data) ->
    SeatID = get_active_seat_id(Data),
    table_msg:broadcast_action(SeatID, 'FOLD', Data),
    NextSeatID = table_dict:get_next_seat(SeatID),
    Data1 = table_util:delete_exist_seat_of_data(SeatID, Data),
    fold_1(Data1, NextSeatID).

fold_1(Data, NextSeatID) ->
    case table_util:is_hand_ending(Data) of
        true ->
            table_state:ending(Data);
        false ->            
            fold_2(Data, NextSeatID)
    end.

fold_2(Data, NextSeatID) ->
    Seat = get_active_seat(Data),    
    NewSeat = Seat#seat{state = 'FOLD'},
    Data1 = table_util:update_seat_of_data(NewSeat, Data),    
    table_util:set_active_seat(NextSeatID, Data1).

%% -------------------------------------------------------------------
%% check
%% -------------------------------------------------------------------
check(Data) ->
    SeatID = get_active_seat_id(Data),
    NextSeatID = table_dict:get_next_seat(SeatID),
    table_msg:broadcast_action(SeatID, 'CHECK', Data),

    Data1 = table_util:delete_round_seat_of_data(SeatID, Data), 
    
    case is_round_ending(Data1) of
        true ->
            table_state:end_of_round(Data1);
        false ->
%%            ?DEBUG("this seat: ~p     next seat: ~p~n", [SeatID, NextSeatID]),
            table_util:set_next_active_seat(NextSeatID, Data1)
    end.   


%% -------------------------------------------------------------------
%% call
%% -------------------------------------------------------------------
call(Data) ->
    Seat = get_active_seat(Data),
    #seat{id = SeatID} = Seat,
    NextSeatID = table_dict:get_next_seat(SeatID),

    Value = get_call_value(Data),
    Data1 = table_pot:bet_pot(SeatID, Value, Data),
    table_msg:broadcast_action(SeatID, 'CALL', Data1),

    case is_round_ending(Data1) of
        true ->
            table_state:end_of_round(Data1);
        false ->
            ?DEBUG("this seat: ~p     next seat: ~p~n", [SeatID, NextSeatID]),
            table_util:set_next_active_seat(NextSeatID, Data1)
    end.    
    

get_call_value(Data) ->
    Seat = get_active_seat(Data),
    #table_data{round_top_bet = RTB} = Data,
    #seat{round_bet = RB, chips = Chips} = Seat,
    {_, Value} = table_util:reduce_chips(Chips, RTB - RB),
    Value.

%% -------------------------------------------------------------------
%% bet or raise
%% -------------------------------------------------------------------
bet(Value, Data) ->
    bet_or_raise(Value, Data, 'BET').

raise(Value, Data) ->
    bet_or_raise(Value, Data, 'RAISE').

bet_or_raise(Value, Data, Flag) ->
    Seat = get_active_seat(Data),
    #seat{id = SeatID} = Seat,
    NextSeatID = table_dict:get_next_seat(SeatID),

    Data1 = table_pot:bet_pot(SeatID, Value, Data),
    table_msg:broadcast_action(SeatID, Flag, Data1),

    Data2 = rest_round_seats(SeatID, Data1),
    ?DEBUG("this seat: ~p     next seat: ~p~n", [SeatID, NextSeatID]),
    table_util:set_next_active_seat(NextSeatID, Data2).

rest_round_seats(SeatID, Data) ->
    #table_data{action_seats = ActiveSeats} = Data,
    RestSeats = lists:delete(SeatID, ActiveSeats),
    Data#table_data{round_seats = RestSeats}.

%% -------------------------------------------------------------------



%% -------------------------------------------------------------------

get_active_seat_id(Data) ->
    #table_data{active_seat = ActiveSeat} = Data,
    {SeatID, _} = ActiveSeat,
    SeatID.

get_active_seat(Data) ->
    SeatID = get_active_seat_id(Data),
    #table_data{seats = Seats} = Data,
    table_util:get_seat(SeatID, Seats).

%% -------------------------------------------------------------------

compare_with_round_top_bet(Seat, Data) ->
    #table_data{round_top_bet = TopBet} = Data,
    #seat{round_bet = SeatBet} = Seat,
    TopBet =:= SeatBet.

%% -------------------------------------------------------------------

fold_or_check(Seat, Data) ->
    case compare_with_round_top_bet(Seat, Data) of
        true ->
            fun check/1;
        false ->
            fun call/1
%%            fun fold/1
    end.

%% -------------------------------------------------------------------

is_round_ending(Data) ->
    #table_data{round_seats = RoundSeats} = Data,
    length(RoundSeats) == 0.
