-ifndef(ACTOR).
-define(ACTOR, true).

-record(actor_data, {
    id,
    type,   %现金或比赛
    total_seats,
    speed,
    sb,
    bb,
    ante,
    min_buy_in,
    max_buy_in,
    seats = [],
    community_cards,
    total_pot
}).

-endif.