-module(table_init).

-include("logger.hrl").
-include("table.hrl").

-export([init/1]).

%%%===================================================================
%%% API
%%%===================================================================
init(ID) ->
    Seats = [
%         #seat{
%             id = 1,
%             state = 'READY',
%             position = 'NONE',
%             actor_id = 101,
%             chips = 2000,
%             round_bet = 0,
%             total_bet = 0,
%             hands = [],
%             is_robot = true
% %%            hands = ["C7","H8"]
%         },
        #seat{
            id = 2,
            state = 'READY',
            position = 'NONE',
            actor_id = 201,
            chips = 2000,
            round_bet = 0,
            total_bet = 0,
            hands = [],
            is_robot = true
%%            hands = ["D7","HA"]
        },
        #seat{
            id = 3,
            state = 'READY',
            position = 'NONE',
            actor_id = 203,
            chips = 2000,
            round_bet = 0,
            total_bet = 0,
            hands = [],
            is_robot = true
%%            hands = ["C2","H9"]
        }
    ],
    Data = #table_data{
        id = ID, seats = Seats, ante = 5, sb = 20, bb = 40, speed = 'FAST', %RAPIDLY
        round_top_bet = 40, min_raise = 40
    },

    % ?DEBUG("TABLE: ~p~n", [Data]),
    {waiting, Data}.

%%%===================================================================
%%% Internal
%%%===================================================================

