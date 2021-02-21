-module(table_msg).

-include("all_pb.hrl").
-include("table.hrl").
-include("logger.hrl").

%%%===================================================================
%%% API
%%%===================================================================

-export([broadcast/2, response/3, broadcast_diff/4, boradcast_exept_hero/3]).
-export([broadcast_active_seat/1]).
-export([broadcast_action/3]).

-export([broadcast_flop/1, broadcast_turn/1, broadcast_river/1]).
-export([broadcast_put_in_blind/1]).
-export([broadcast_end_of_round/1]).
-export([broadcast_show_hand/1, broadcast_win/1]).
-export([broadcast_ready_state/1]).
-export([broadcast_actor_sit_down/2, broadcast_seat_state/2]).

-export([get_enter_info/3]).

%%%-------------------------------------------------------------------
%%% common
%%%-------------------------------------------------------------------
broadcast(Cmd, Data) ->
    List = table_dict:get_all_player(),
    main:broadcast(List, Cmd, Data).

response(PlayerID, Cmd, Data) ->
    main:response(PlayerID, Cmd, Data).

broadcast_diff(HeroID, Cmd, HeroData, OppData) ->
    List = table_dict:get_all_player(),
    Others = lists:delete(HeroID, List),
    main:broadcast(Others, Cmd, OppData),
    case lists:member(HeroID, List) of
        true ->
            response(HeroID, Cmd, HeroData);
        false ->
            ok
    end.

boradcast_exept_hero(HeroID, Cmd, Data) ->
    List = table_dict:get_all_player(),
    Others = lists:delete(HeroID, List),
    main:broadcast(Others, Cmd, Data).


%%%-------------------------------------------------------------------
%%% spec msg
%%%-------------------------------------------------------------------

broadcast_action(SeatID, Action, Table) ->
    Cmd = 2029,
    Data = format_action(SeatID, Action, Table),
    broadcast(Cmd, Data).

broadcast_active_seat(SeatID) ->
    Cmd = 2010,
    Data = #m_2010_toc{seat_id = SeatID},
    broadcast(Cmd, Data).

broadcast_flop(Cards) ->
    Cmd = 2021,
    Data = #m_2021_toc{cards = Cards},
    broadcast(Cmd, Data).

broadcast_turn(Card) ->
    Cmd = 2022,
    Data = #m_2022_toc{card = Card},
    broadcast(Cmd, Data).

broadcast_river(Card) ->
    Cmd = 2022,
    Data = #m_2022_toc{card = Card},
    broadcast(Cmd, Data).

broadcast_end_of_round(Info) ->
    {CurrentPot, SidePot, SeatsChips} = format_end_of_round(Info),
    Cmd = 2023,
    Data = #m_2023_toc{
        current_pot = CurrentPot,
        side_pot = SidePot,
        seats_chips = SeatsChips
    },
    broadcast(Cmd, Data).

broadcast_put_in_blind(Table) ->
    #table_data{
        seats = Seats,
        total_pot = TotalPot,
        current_pot = CurrentPot
    } = Table,
    Chips = format_seats_chips(Seats),
    Cmd = 2024,
    Data = #m_2024_toc{
        seats_chips = Chips,
        total_pot = TotalPot,
        current_pot = CurrentPot
    },
    broadcast(Cmd, Data).

broadcast_show_hand(Table) ->
    List = table_util:get_exist_hand_of_data(Table),
    Hands = format_seats_hand(List),
    Cmd = 2025,
    Data = #m_2025_toc{hands = Hands},
    broadcast(Cmd, Data).

broadcast_win(Table) ->
    Info = table_dict:get_win_pot_record(),
    Seats = format_win(Table, Info),
    Cmd = 2026,
    Data = #m_2026_toc{seats = Seats},
    broadcast(Cmd, Data).

broadcast_ready_state(TableData) ->
    Cmd = 2030,
    #table_data{seats = Seats, total_pot = TotalPot, current_pot = CurrentPot} = TableData,
    Players = table_dict:get_all_player(),
    lists:foreach(fun(X) ->
        SeatsInfo = format_seats(Seats, X),
        Data = #m_2030_toc{seats = SeatsInfo, total_pot = TotalPot, current_pot = CurrentPot},
        response(X, Cmd, Data)
    end, Players).

broadcast_actor_sit_down(Seat, HeroID) ->
    HeroSeat = format_seat(Seat, HeroID),
    HeroData = #m_2002_toc{seat = HeroSeat},
    response(HeroID, 2002, HeroData),

    OppSeat = format_seat(Seat, undefined),
    OppData = #m_2027_toc{seat = OppSeat},
    boradcast_exept_hero(HeroID, 2027, OppData).


broadcast_seat_state(SeatID, State) ->
    SeatState = #p_seat_state{seat_id = SeatID, state = State},
    Data = #m_2028_toc{seat_state = SeatState},
    Cmd = 2028,
    broadcast(Cmd, Data).



%%%-------------------------------------------------------------------
%%% request msg
%%%-------------------------------------------------------------------
get_enter_info(State, Info, HeroID) ->
    format_table_info(State, Info, HeroID).


%%%===================================================================
%%% Internal
%%%===================================================================


% --------------------------------------------------------------------
% format_seats_chips
% --------------------------------------------------------------------
format_seats_chips(Seats) ->
    lists:foldl(fun(Seat, Acc) ->
        Info = format_seat_chips(Seat),
        [Info | Acc]
    end, [], Seats).

format_seat_chips(Seat) ->
    #seat{id = SeatID, chips = Chips, round_bet = RoundBet} = Seat,
    #p_seat_chips{
        seat_id = SeatID,
        chips = RoundBet,
        actor_chips = Chips
    }.

% --------------------------------------------------------------------
% format_table_info
% --------------------------------------------------------------------
format_table_info(State, Info, HeroID) ->
    #table_data{
        id = ID,
        game_type = GameType,
        type_of_play = TypeOfPlay,
        total_seats = TotalSeat,
        speed = Speed,
        sb = SB,
        bb = BB,
        ante = Ante,
        min_buy_in = MinBuyIn,
        max_buy_in = MaxBuyIn,
        seats = Seats,
        community_cards = CommunityCards,
        total_pot = TotalPot,
        current_pot = CurrentPot,
        side_pot = SidePot,
        active_seat = AS
    } = Info,

    ActiveSeat = format_active_seat(AS, Speed),
    NewSeats = format_seats(Seats, HeroID),
    PSidePot = format_side_pot(SidePot),
    #p_table{
        id = ID,
        game_type = GameType,
        type_of_play = TypeOfPlay,
        total_seats = TotalSeat,
        speed = Speed,
        sb = SB,
        bb = BB,
        ante = Ante,
        min_buy_in = MinBuyIn,
        max_buy_in = MaxBuyIn,
        seats = NewSeats,
        community_cards = CommunityCards,
        total_pot = TotalPot,
        current_pot = CurrentPot,
        side_pot = PSidePot,
        active_seat = ActiveSeat,
        state = State
    }.

% --------------------------------------------------------------------
% format_active_seat
% --------------------------------------------------------------------
format_active_seat({ID, Timestamp}, Speed) ->
    Sec = table_util:get_speed_sec(Speed),
    Now = time:now(),
    #p_active_seat{id = ID, remain_sec = math2:zero_valid(Sec - (Now - Timestamp))}.

% --------------------------------------------------------------------
% format_seats
% --------------------------------------------------------------------
format_seats(Seats, HeroID) ->
    lists:map(fun(X) ->
        format_seat(X, HeroID)
    end, Seats).

format_seat(Seat, HeroID) ->
    #seat{
        id = ID,
        state = State,
        position = Position,
        actor_id = ActorID,
        chips = Chips,
        round_bet = RoundBet,
        total_bet = TotalBet,
        hands = Hands
    } = Seat,

    {Name, NewHands, ActorType} = get_actor_property(ActorID, Hands, HeroID),

    Actor = #p_actor{
        id = ActorID,
        name = Name,
        state = State,
        position = Position,
        chips = Chips,
        round_bet = RoundBet,
        total_bet = TotalBet,
        hands = NewHands
    },

    #p_seat{
        id = ID,
        type = ActorType,
        actor = Actor
    }.

get_actor_property(ActorID, Hands, HeroID) ->
    case ActorID of
        HeroID ->
            {"Hero", Hands, 'HERO'};
        _ ->
            NewHands = case Hands of
                           [] ->
                               [];
                           _ ->
                               ["U", "U"]
                       end,
            {"Opponent"++integer_to_list(ActorID), NewHands, 'OPPONENT'}
    end.

% --------------------------------------------------------------------
% format_end_of_round
% --------------------------------------------------------------------
format_end_of_round(Data) ->
    #table_data{
        seats = Seats,
        current_pot = CurrentPot,
        side_pot = SidePot
    } = Data,
    SeatsChips = format_seats_chips(Seats),
    PSidePot = format_side_pot(SidePot),
    {CurrentPot, PSidePot, SeatsChips}.

% --------------------------------------------------------------------
% format_side_pot
% --------------------------------------------------------------------
format_side_pot(List) ->
    lists:map(fun(X) ->
        {ID, Pot} = X,
        #p_side_pot{id = ID, pot = Pot}
    end, List).

% --------------------------------------------------------------------
% format_side_pot
% --------------------------------------------------------------------
format_seats_hand(Seats) ->
    lists:map(fun(X) ->
        format_seat_hand(X)
    end, Seats).

format_seat_hand(Seat) ->
    {ID, Hands} = Seat,
    #p_seat_hand{
        seat_id = ID,
        cards = Hands
    }.

% --------------------------------------------------------------------
% format_win
% --------------------------------------------------------------------
format_win(Table, List) ->
    #table_data{seats = Seats} = Table,
    lists:map(fun(X) ->
        {SeatId, Pot, Ids} = X,
        Seat = table_util:get_seat(SeatId, Seats),
        #p_win_pot{
            seat_id = SeatId,
            pot = Pot,
            pot_ids = Ids,
            chips = Seat#seat.chips
        }
    end, List).

% --------------------------------------------------------------------
% format_action
% --------------------------------------------------------------------
format_action(SeatID, Action, Table) ->
    Data = #m_2029_toc{seat_id = SeatID, action_type = Action},
    case Action of
        A when A == 'CALL' orelse A == 'BET' orelse A == 'RAISE' ->
            Seat = table_util:get_seat_of_data(SeatID, Table),
            Data#m_2029_toc{
                chips = Seat#seat.chips,
                pot = Seat#seat.round_bet,
                total_pot = Table#table_data.total_pot,
                current_pot = Table#table_data.current_pot
            };
        _ ->
            Data
    end.
