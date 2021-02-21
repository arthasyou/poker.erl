-ifndef(TABLE).
-define(TABLE, true).

-record(table_data, {
    id,
    game_type = 'HOLDEM',    % hold'em, omaha ...
    type_of_play = 'CASH',   % cash, tournament ...
    total_seats = 9,
    speed = 'NORMAL',
    sb = 1,
    bb = 2,
    ante = 0,
    min_buy_in = 20,
    max_buy_in = 100,
    seats = [],
    community_cards = [],
    total_pot = 0,
    current_pot = 0,
    side_pot = [],
    total_side_pot = 0,
    next_side_pot_id = 1,
    round_top_bet = 0,          
    min_raise = 0,              
    last_btn_seat = 1,
    exist_seats = [],
    action_seats = [],
    round_start_seats = [],     % for side pot
    round_seats = [],           % check for round end
    active_seat = {0, 0},
    remain_cards = []
}).

-record(seat, {
    id,
    state = 'SIT_OUT',
    position = 'NONE',
    actor_id,
    chips = 0,
    round_bet = 0,
    total_bet = 0,
    hands = [],
    is_robot = false
}).

-endif.