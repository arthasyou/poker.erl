-module(table_pot).

-include("table.hrl").
-include("logger.hrl").

%%%===================================================================
%%% API
%%%===================================================================

-export([ante_pot/1, round_side_pot/1]).
-export([bet_pot/3, blind_pot/1]).
-export([get_seat_pot/2, get_single_pot/1]).
-export([get_multi_pot_seat_id/1]).

-define(ALL_SIDE_POT, 9).

%---------------------------------------------------------------------
% ante pot
%---------------------------------------------------------------------
ante_pot(Data) ->
    #table_data{
        seats = Seats,
        ante = Ante,
        exist_seats = ExistSeats,
        next_side_pot_id = SidePotID
    } = Data,
    
    {NewSeats, TotalPot, ActionSeats, SidePotKeys} = 
    lists:foldl(fun(X, {AccSeat, AccPot, AccAS, AccSpk}) ->
        Seat = table_util:get_seat(X, AccSeat),
        #seat{id = SeatID, chips = Chips} = Seat,                   
        {NewChips, Bet} = table_util:reduce_chips(Chips, Ante),
        NewAccSeat = update_seats(Seat, NewChips, Bet, AccSeat),
        NewAccAS = delete_from_active_seat(SeatID, NewChips, AccAS),
        NewAccSpk = update_side_pot_keys(SeatID, NewChips, Bet, AccSpk),
        {NewAccSeat, AccPot + Bet, NewAccAS, NewAccSpk}        
    end, {Seats, 0, ExistSeats, []}, ExistSeats),

    Data1 = Data#table_data{
        seats = NewSeats,
        total_pot = TotalPot,
        action_seats = ActionSeats,
        round_start_seats = ActionSeats,
        round_seats = ActionSeats
    },

    case length(SidePotKeys) of
        0 ->
            Data1#table_data{current_pot = TotalPot};
        _ ->
            {SidePot, TotalSidePot, NextSidePotID}
            = side_pot(SidePotKeys, [], length(ExistSeats), SidePotID),
            CurrentPot = TotalPot - TotalSidePot,
            Data1#table_data{
                side_pot = SidePot,
                total_side_pot = TotalSidePot,
                current_pot = CurrentPot,
                next_side_pot_id = NextSidePotID
            }
    end.    

update_seats(Seat, Chips, Bet, Seats) ->
    NewSeat = Seat#seat{chips = Chips, total_bet = Bet},
    table_util:replace_seat(NewSeat, Seats).

delete_from_active_seat(SeatID, Chips, ActiveSeats) ->
    case Chips of
        0 ->
            table_dict:delete_seat(SeatID),
            lists:delete(SeatID, ActiveSeats);
        _ ->
            ActiveSeats
    end.

update_side_pot_keys(SeatID, Chips, Bet, Keys) ->
    case Chips of
        0 ->
            [{SeatID, Bet} | Keys];
        _ ->
            Keys
    end.

%---------------------------------------------------------------------
% Round Pot
%---------------------------------------------------------------------
round_side_pot(Data) ->
    #table_data{
        seats = Seats,
        round_start_seats = RoundStartSeats,
        action_seats = ActionSeats,
        side_pot = SidePot,
        next_side_pot_id = SidePotID,
        bb = BB
    } = Data,

    {TotalPot, SidePotKeys} = 
    lists:foldl(fun(X, {AccPot, AccSpk}) ->
        Seat = table_util:get_seat(X, Seats),
        #seat{id = SeatID, chips = Chips, round_bet = Bet} = Seat,
        
        NewAccSpk = update_side_pot_keys(SeatID, Chips, Bet, AccSpk),
        {AccPot + Bet, NewAccSpk}        
    end, {0, []}, RoundStartSeats),

    Data1 = Data#table_data{
        round_top_bet = 0,
        min_raise = BB,
        round_start_seats = ActionSeats,
        round_seats = ActionSeats   
    },
    case length(SidePotKeys) of
        0 ->
            Data1#table_data{current_pot = TotalPot};
        _ ->
            {NewSidePot, TotalSidePot, NextSidePotID} = 
            side_pot(SidePotKeys, SidePot, length(RoundStartSeats), SidePotID),
            CurrentPot = TotalPot - TotalSidePot,
            Data1#table_data{
                side_pot = NewSidePot,
                total_side_pot = TotalSidePot,
                current_pot = CurrentPot,
                next_side_pot_id = NextSidePotID
            }
    end.

%---------------------------------------------------------------------
% blind pot
%---------------------------------------------------------------------
blind_pot(Data) ->
    Data1 = sb_pot(Data),
    Data2 = bb_pot(Data1),
    #table_data{action_seats = ActionSeats} = Data2,
    Data2#table_data{
        round_seats = ActionSeats
    }.

sb_pot(Data) ->
    case get_blind_seat_and_value(sb, Data) of
        undefined ->
            Data;
        {SeatID, Bet} ->
            bet_pot(SeatID, Bet, Data)
    end.

bb_pot(Data) ->
    case get_blind_seat_and_value(bb, Data) of
        undefined ->
            Data;
        {SeatID, Bet} ->
            bet_pot(SeatID, Bet, Data)
    end.

get_blind_seat_and_value(Type, Data) ->
    #table_data{
        exist_seats = ExistSeats,
        action_seats = ActionSeat,
        sb = SB,
        bb = BB
    } = Data,
    Len = length(ExistSeats),
    Nth = get_pos_nth(Type, Len),
    SeatID = lists:nth(Nth, ExistSeats),
    case lists:member(SeatID, ActionSeat) of
        true ->
            Value = case Type of
                sb ->
                   SB; 
                bb ->
                   BB
            end,
            {SeatID, Value};
        false ->
            undefined
    end.

get_pos_nth(Pos, Len) ->
    case Pos of
        sb ->
            case Len of
                2 ->
                    2;
                _ ->
                    1
            end;
        bb ->
            case Len of
                2 ->
                    1;
                _ ->
                    2
            end
    end.

%---------------------------------------------------------------------
% Bet Pot
%---------------------------------------------------------------------
bet_pot(SeatID, Bet, Data) ->
    Seat = table_util:get_seat_of_data(SeatID, Data),
    #seat{
        chips = Chips,
        total_bet = TotalBet,
        round_bet = RoundBet
    } = Seat,
    {NewChips, NewBet} = table_util:reduce_chips(Chips, Bet),
    NewTotalBet = TotalBet + NewBet,
    NewRoundBet = RoundBet + NewBet,
    NewSeat = Seat#seat{chips = NewChips, total_bet = NewTotalBet, round_bet = NewRoundBet},
    Data1 = table_util:update_seat_of_data(NewSeat, Data),
    Data2 = table_util:delete_round_seat_of_data(SeatID, Data1),
    Data3 = check_delete_action_seats(SeatID, NewChips, Data2),
    table_util:increase_pot_of_data(Data3, NewBet, NewRoundBet).


check_delete_action_seats(SeatID, Chips, Data) ->
    #table_data{action_seats = ActionSeats} = Data,
    NewActionSeats = delete_from_active_seat(SeatID, Chips, ActionSeats),
    Data#table_data{action_seats = NewActionSeats}.


%%%===================================================================
%%% Intenal
%%%===================================================================

%---------------------------------------------------------------------
% side pot
%---------------------------------------------------------------------
side_pot(Keys, SidePot, NumberOfPlayer, SidePotID) ->
    List = sort_side_pot_keys(Keys),
    {NewSidePot, NextSidePotID, _, _} = 
    lists:foldl(fun(X, {AccSp, AccID, AccLastBet, AccLen}) ->
        {SeatID, Bet} = X,
        case AccLen of
            1 ->
                {AccSp, AccID, Bet, AccLen};
            _ ->
                Pot = (Bet - AccLastBet) * AccLen,
                table_dict:put_side_pot(SeatID, AccID),
                {[{AccID, Pot} | AccSp], AccID + 1, Bet, AccLen - 1}
        end
    end, {SidePot, SidePotID, 0, NumberOfPlayer}, List),
    TotalSidePot = get_total_side_pot(NewSidePot),
    {NewSidePot, TotalSidePot, NextSidePotID}.

sort_side_pot_keys(Keys) ->
    lists:sort(fun({_K1, V1}, {_K2, V2}) ->
        V1 < V2
    end, Keys).

get_total_side_pot(SidePot) ->
    lists:foldl(fun(X, Acc) ->
        {_, V} = X,
        Acc + V
    end, 0, SidePot).

% --------------------------------------------------------------------
% get single pot
% --------------------------------------------------------------------
get_single_pot(Data) ->
    #table_data{
        total_pot = TotalPot,
        side_pot = SidePot,
        current_pot = CurrentPot
    } = Data,
    NewData = Data#table_data{
        total_pot = 0,
        current_pot = 0,
        side_pot = [],
        exist_seats = []
    },
    {TotalPot, [{0, CurrentPot} | SidePot], NewData, done}.

% --------------------------------------------------------------------
% get seat pot
% --------------------------------------------------------------------
get_seat_pot(SeatID, Data) ->
    #table_data{total_pot = TotalPot} = Data,
    case table_dict:get_side_pot_ids(SeatID) of
        undefined ->
            get_single_pot(Data);
        IDS ->
            #table_data{
                side_pot = SidePot,
                exist_seats = ExistSeats
            } = Data,
            {TakeSidePot, RestSidePot} = pick_pot(IDS, SidePot),
            Value = get_side_pot_value(TakeSidePot),
            NewTotalPot = TotalPot - Value,
            NewExistSeats = get_rest_exist_seats(ExistSeats, IDS),
            NewData = Data#table_data{
                total_pot = NewTotalPot,
                side_pot = RestSidePot,
                exist_seats = NewExistSeats
            },
            Flag = get_side_pot_flag(RestSidePot, NewTotalPot),
            {Value, TakeSidePot, NewData, Flag}
    end.

pick_pot(Ids, SidePot) ->    
    TakePot = lists:foldl(fun(X, Acc) ->
        case lists:keyfind(X, 1, SidePot) of
            false ->
                Acc;
            Item ->
                [Item | Acc]
        end
    end, [], Ids),
    {TakePot, SidePot -- TakePot}.

get_rest_exist_seats(ExistSeats, SidePot) ->    
    lists:foldl(fun(X, Acc) ->
        SeatID = table_dict:get_side_pot_seat(X),
        lists:delete(SeatID, Acc)
    end, ExistSeats, SidePot).

get_side_pot_value(SidePot) ->
    lists:foldl(fun(X, Acc) ->
        {_, V} = X,
        V + Acc
    end, 0, SidePot).

get_side_pot_flag(SidePot, TotalPot) ->
    case SidePot of
        [] ->
            case TotalPot of
                0 ->
                    done;
                _ ->
                    empty
            end;            
        _ ->
            continued
    end.

% --------------------------------------------------------------------
% get multi seat pot
% --------------------------------------------------------------------
get_multi_pot_seat_id(List) ->
    [Head | Tail] = List,
    HeadLen = get_seat_side_pot_length(Head),
    {DesID, _} =
    lists:foldl(fun(X, {AccID, AccLen}) ->
        Len = get_seat_side_pot_length(X),
        case Len < AccLen of
            true ->
                {X, Len};
            false ->
                {AccID, AccLen}
        end
    end, {Head, HeadLen}, Tail),
    DesID.

get_seat_side_pot_length(SeatID) ->
    case table_dict:get_side_pot_ids(SeatID) of
        undefined ->
            ?ALL_SIDE_POT;
        IDS ->
            length(IDS)
    end.