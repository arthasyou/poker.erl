-module(table_state).

-include("table.hrl").
-include("logger.hrl").

%%%===================================================================
%%% API
%%%===================================================================
-export([waiting/1, ready/1]).
-export([ante/1, blind/1, preflop/1, flop/1, turn/1, river/1, showdown/1]).
-export([show_run/1]).
-export([end_of_round/1, ending/1]).

%% -------------------------------------------------------------------
%% state behavior
%% -------------------------------------------------------------------

waiting(Data) ->
%%    ?DEBUG("waiting: ~p~n", [Data]),
    waiting_action:start(Data).

ready(Data) ->
%%    ?DEBUG("ready: ~p~n", [Data]),
    ready_action:start(Data).

ante(Data) ->
%%    ?DEBUG("blind: ~p~n", [Data]),
    ante_action:start(Data).

blind(Data) ->
%%    ?DEBUG("blind: ~p~n", [Data]),
    blind_action:start(Data).

preflop(Data) ->
%%    ?DEBUG("preflop: ~p~n", [Data]),
    preflop_action:start(Data).

flop(Data) ->
%%    ?DEBUG("flop: ~p~n", [Data]),
    flop_action:start(Data).

turn(Data) ->
%%    ?DEBUG("turn: ~p~n", [Data]),
    turn_action:start(Data).

river(Data) ->
%%    ?DEBUG("river: ~p~n", [Data]),
    river_action:start(Data).

showdown(Data) ->
%%    ?DEBUG("showdown: ~p~n", [Data]),
    showdown_action:start(Data).

show_run(Data) ->
    show_run_action:start(Data).

end_of_round(Data) ->
%%    ?DEBUG("end_of_round: ~p~n", [Data]),
    common_action:end_of_round(Data).

ending(Data) ->
%%    ?DEBUG("ending: ~p~n", [Data]),
    common_action:ending(Data).