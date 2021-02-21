-module(holdem_callback).

-include("logger.hrl").
-include("table.hrl").
-include("all_pb.hrl").

%%%===================================================================
%%% API
%%%===================================================================
-export([enter_table/2, sit_down/2, ready/2]).
-export([fold/2, check_call/2, bet_raise/2]).

%% -------------------------------------------------------------------
%% enter_table
%% -------------------------------------------------------------------
enter_table(_DataIn, HeroID) ->
    case table:enter_table(HeroID, 2) of
        {ok, Info} ->
            main_dict:enter_table(2),
            {ok, #m_2000_toc{info = Info}};
        fail ->
            {error, 1}
    end.

%% -------------------------------------------------------------------
%% enter_table
%% -------------------------------------------------------------------
sit_down(DataIn, HeroID) ->
    #m_2002_tos{
        table_id = TableID,
        seat_id = SeatID
    } = DataIn,
    case table_action:sit_down(HeroID, TableID, SeatID) of
        {error, ErrCode} ->
            {error, ErrCode};
        _ ->
            noreply
    end.

%% -------------------------------------------------------------------
%% ready
%% -------------------------------------------------------------------
ready(DataIn, HeroID) ->
    #m_2003_tos{
        table_id = TableID
    } = DataIn,
    case table_action:ready(HeroID, TableID) of
        {error, ErrCode} ->
            {error, ErrCode};
        _ ->
            noreply
    end.

% -------------------------------------------------------------------
%% fold
%% -------------------------------------------------------------------
fold(DataIn, HeroID) ->
    #m_2011_tos{
        table_id = TableID
    } = DataIn,
    case table_action:fold(HeroID, TableID) of
        {error, ErrCode} ->
            {error, ErrCode};
        _ ->
            noreply
    end.

% -------------------------------------------------------------------
%% check_call
%% -------------------------------------------------------------------
check_call(DataIn, HeroID) ->
    #m_2012_tos{
        table_id = TableID
    } = DataIn,
    case table_action:check_call(HeroID, TableID) of
        {error, ErrCode} ->
            {error, ErrCode};
        _ ->
            noreply
    end.

% -------------------------------------------------------------------
%% bet_raise
%% -------------------------------------------------------------------
bet_raise(DataIn, HeroID) ->
    #m_2013_tos{
        table_id = TableID,
        chips = Chips
    } = DataIn,
    case table_action:bet_raise(HeroID, TableID, Chips) of
        {error, ErrCode} ->
            {error, ErrCode};
        _ ->
            noreply
    end.


%%%===================================================================
%%% Internal
%%%===================================================================





