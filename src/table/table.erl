-module(table).

-include("logger.hrl").
-include("error_code.hrl").

%%%===================================================================
%%% API
%%%===================================================================

-export([start/0, spawn_table/0]).
-export([enter_table/2, leave_table/2]).
-export([sit_down/3, ready/2]).
-export([fold/2, check_call/2, bet_raise/3]).


start() ->
    counter:init(table_id),
    spawn_table().

spawn_table() ->
    ID = counter:up(table_id),
    table_sup:start_child([ID]).

enter_table(PlayerID, TableID) ->
    case table_mgr:get_pid(TableID) of
        {ok, PID} ->
            gen_statem:call(PID, {enter_table, PlayerID});
        _ ->
            {error, ?SYS}
    end.

leave_table(PlayerID, TableID) ->
    case table_mgr:get_pid(TableID) of
        {ok, PID} ->
            gen_statem:cast(PID, {leave_table, PlayerID});
        _ ->
            {error, ?SYS}
    end.

sit_down(ActorID, TableID, SeatID) ->
    case table_mgr:get_pid(TableID) of
        {ok, PID} ->
            Msg = {player_action, sit_down, [ActorID, SeatID]},
            gen_statem:call(PID, Msg);
        _ ->
            {error, ?SYS}
    end.

ready(ActorID, TableID) ->
    case table_mgr:get_pid(TableID) of
        {ok, PID} ->
            Msg = {player_action, ready, [ActorID]},
            gen_statem:call(PID, Msg);
        _ ->
            {error, ?SYS}
    end.

fold(ActorID, TableID) ->
    case table_mgr:get_pid(TableID) of
        {ok, PID} ->
            Msg = {action, ActorID, fold},
            gen_statem:call(PID, Msg);
        _ ->
            {error, ?SYS}
    end.

check_call(ActorID, TableID) ->
    case table_mgr:get_pid(TableID) of
        {ok, PID} ->
            Msg = {action, ActorID, check_call},
            gen_statem:call(PID, Msg);
        _ ->
            {error, ?SYS}
    end.

bet_raise(ActorID, TableID, Value) ->
    case table_mgr:get_pid(TableID) of
        {ok, PID} ->
            Msg = {action, ActorID, {bet_raise, Value}},
            gen_statem:call(PID, Msg);
        _ ->
            {error, ?SYS}
    end.
