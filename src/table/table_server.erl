%%%-------------------------------------------------------------------
%%% @author luobin
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Nov 2020 3:31 PM
%%%-------------------------------------------------------------------
-module(table_server).
-author("luobin").
-include("logger.hrl").
-include("table.hrl").

-behaviour(gen_statem).

%% API
-export([start_link/1]).

%% gen_statem callbacks
-export([init/1, format_status/2, handle_event/4, terminate/3,
    code_change/4, callback_mode/0]).

%% state_name
-export([waiting/3, ready/3, ante/3, blind/3, preflop/3, flop/3, turn/3, river/3, showdown/3, show_run/3]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================
start_link(ID) ->
    gen_statem:start_link(?MODULE, [ID], []).


%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

init([ID]) ->
    process_flag(trap_exit, true),
    table_mgr:register(ID, self()),
    {State, Data} = table_init:init(ID),    
    {ok, State, Data}.

%% @private
%% @doc This function is called by a gen_statem when it needs to find out
%% the callback mode of the callback module.
callback_mode() ->
    [state_functions, state_enter].
    % handle_event_function.

%% @private
%% @doc Called (1) whenever sys:get_status/1,2 is called by gen_statem or
%% (2) when gen_statem terminates abnormally.
%% This callback is optional.
format_status(_Opt, [_PDict, _State, _Data]) ->
    Status = some_term,
    Status.

%% @private
%% @doc There should be one instance of this function for each possible
%% state name.  If callback_mode is state_functions, one of these
%% functions is called when gen_statem receives and event from
%% call/2, cast/2, or as a normal process message.

%% -----------------------------------------------------------------------
%% waiting state
%% -----------------------------------------------------------------------
waiting(enter, _OldState, Data) ->
    {NextState, NewData} = table_state:waiting(Data),
    case NextState of
        ready ->
            {keep_state, NewData, [{state_timeout, 0, NextState}]};
        waiting ->
            {keep_state, NewData}
    end;
waiting(state_timeout, ready, Data) ->
    {next_state, ready, Data};
waiting(EventType, EventContent, Data) ->
    all_state(EventType, EventContent, waiting, Data).

%% -----------------------------------------------------------------------
%% ready state
%% -----------------------------------------------------------------------
ready(enter, _OldState, Data) ->
    {enter, NewData} = table_state:ready(Data),
    {keep_state, NewData, [{state_timeout, 0, enter}]};
ready(state_timeout, enter, Data) ->
    {next_state, ante, Data};
ready(EventType, EventContent, Data) ->
    all_state(EventType, EventContent, ante, Data).

%% -----------------------------------------------------------------------
%% ante state
%% -----------------------------------------------------------------------
ante(enter, _OldState, Data) ->
    {enter, NewData} = table_state:ante(Data),
    {keep_state, NewData, [{state_timeout, 1000, enter}]};
ante(state_timeout, enter, Data) ->
    {next_state, blind, Data};
ante(EventType, EventContent, Data) ->
    all_state(EventType, EventContent, ante, Data).

%% -----------------------------------------------------------------------
%% ready state
%% -----------------------------------------------------------------------
blind(enter, _OldState, Data) ->
    {enter, NewData} = table_state:blind(Data),
    {keep_state, NewData, [{state_timeout, 1000, enter}]};
blind(state_timeout, enter, Data) ->
    {next_state, preflop, Data};
blind(EventType, EventContent, Data) ->
    all_state(EventType, EventContent, blind, Data).



%% -----------------------------------------------------------------------
%% preflop state
%% -----------------------------------------------------------------------
preflop(enter, _OldState, Data) ->
    case table_state:preflop(Data) of
        {{timeout, Timeout}, NewData} ->
            {keep_state, NewData, [{{timeout, preflop}, Timeout, action}]};
        {{next_state, NextState}, NewData} ->
            {keep_state, NewData, [{{timeout, NextState}, 1000, action}]}
    end;    
preflop({timeout, preflop}, action, Data) ->
    action_state({timeout, preflop}, action, Data);
%%    case actor_action:timeout(Data) of
%%        {{timeout, Timeout}, NewData} ->
%%            {keep_state, NewData, [{{timeout, preflop}, Timeout, action}]};
%%        {{next_state, NextState}, NewData} ->
%%            {next_state, NextState, NewData};
%%        {next_round, NewData} ->
%%            {next_state, flop, NewData}
%%    end;
%%preflop({timeout, preflop}, cancel, Data) ->
%%    ?DEBUG("timeout: ~p~n", [Data]),
%%    keep_state_and_data;
preflop(EventType, EventContent, Data) ->
    action_state(EventType, EventContent, preflop, Data).

%% -----------------------------------------------------------------------
%% flop state
%% -----------------------------------------------------------------------
flop(enter, _OldState, Data) ->
    {{timeout, Timeout}, NewData} = table_state:flop(Data),
    {keep_state, NewData, [{{timeout, flop}, Timeout, action}]};
flop({timeout, flop}, action, Data) ->
    action_state({timeout, flop}, action, Data);
%%    case actor_action:timeout(Data) of
%%        {{timeout, Timeout}, NewData} ->
%%            {keep_state, NewData, [{{timeout, flop}, Timeout, action}]};
%%        {{next_state, NextState}, NewData} ->
%%            {next_state, NextState, NewData};
%%        {next_round, NewData} ->
%%            {next_state, turn, NewData}
%%    end;
flop(EventType, EventContent, Data) ->
    action_state(EventType, EventContent, flop, Data).

%% -----------------------------------------------------------------------
%% turn state
%% -----------------------------------------------------------------------
turn(enter, _OldState, Data) ->
    {{timeout, Timeout}, NewData} = table_state:turn(Data),
    {keep_state, NewData, [{{timeout, turn}, Timeout, action}]};
turn({timeout, turn}, action, Data) ->
    action_state({timeout, turn}, action, Data);
%%    case actor_action:timeout(Data) of
%%        {{timeout, Timeout}, NewData} ->
%%            {keep_state, NewData, [{{timeout, turn}, Timeout, action}]};
%%        {{next_state, NextState}, NewData} ->
%%            {next_state, NextState, NewData};
%%        {next_round, NewData} ->
%%            {next_state, river, NewData}
%%    end;
turn(EventType, EventContent, Data) ->
    action_state(EventType, EventContent, turn, Data).

%% -----------------------------------------------------------------------
%% river state
%% -----------------------------------------------------------------------
river(enter, _OldState, Data) ->
    {{timeout, Timeout}, NewData} = table_state:river(Data),
    {keep_state, NewData, [{{timeout, river}, Timeout, action}]};
river({timeout, river}, EventContent, Data) ->
    action_state({timeout, river}, EventContent, Data);
%%    case actor_action:timeout(Data) of
%%        {{timeout, Timeout}, NewData} ->
%%            {keep_state, NewData, [{{timeout, river}, Timeout, action}]};
%%        {{next_state, NextState}, NewData} ->
%%            {next_state, NextState, NewData};
%%        {next_round, NewData} ->
%%            {next_state, showdown, NewData}
%%    end;
river(EventType, EventContent, Data) ->
    action_state(EventType, EventContent, river, Data).

%% -----------------------------------------------------------------------
%% showdown state
%% -----------------------------------------------------------------------
showdown(enter, _OldState, Data) ->
    NewData = table_state:showdown(Data),
    {keep_state, NewData, [{state_timeout, 5000, waiting}]};
showdown(state_timeout, waiting, Data) ->
    {next_state, waiting, Data};
showdown(EventType, EventContent, Data) ->
    all_state(EventType, EventContent, showdown, Data).

%% -----------------------------------------------------------------------
%% show_run state
%% -----------------------------------------------------------------------
show_run(enter, _OldState, Data) ->
    Action = table_state:show_run(Data),
    {keep_state, Data, [{state_timeout, 1000, Action}]};
show_run(state_timeout, Action, Data) ->
    {action, NextAction, NewData} = show_run_action:Action(Data),
    {keep_state, NewData, [NextAction]};
show_run({timeout, show_run}, Action, Data) ->
    case show_run_action:Action(Data) of
        {action, NextAction, NewData} ->
            {keep_state, NewData, [NextAction]};
        {{next_state, NextState}, NewData} ->
            {next_state, NextState, NewData}
    end;
show_run(EventType, EventContent, Data) ->
    all_state(EventType, EventContent, show_run, Data).

%% -----------------------------------------------------------------------
%% action state
%% -----------------------------------------------------------------------
action_state({timeout, State}, action, Data) ->
    case actor_action:timeout(Data) of
        {{timeout, Timeout}, NewData} ->
            {keep_state, NewData, [{{timeout, State}, Timeout, action}]};
        {{next_state, NextState}, NewData} ->
            {next_state, NextState, NewData};
        {next_round, NewData} ->
            NextState = get_next_round(State),
            {next_state, NextState, NewData}
    end.



%%action_state(EventType, NextState, Data) ->
%%    ?DEBUG("cancel: ~p~n", [NextState]),
%%    {next_state, NextState, Data}.

%%action_state({call, From}, {action, PlayerID, Action}, State, Data) ->
%%    ?DEBUG("going here: ~p~n", [Action]),
%%    {keep_state_and_data, [{{timeout, State}, cancel}, {reply, From, ok}]};
action_state({call, From}, {action, PlayerID, Action}, State, Data) ->
    case player_action:action(PlayerID, Action, Data) of
        {ok, {Callback, NewData}} ->
            CallReply = {reply, From, ok},
            case Callback of
                {timeout, Timeout} ->
                    {keep_state, NewData, [{{timeout, State}, Timeout, action}, CallReply]};
                {next_state, NextState} ->
                    {next_state, NextState, NewData, [{{timeout, State}, cancel}, CallReply]};
                next_round ->
                    NextState = get_next_round(State),
                    {next_state, NextState, NewData, [{{timeout, State}, cancel}, CallReply]}
            end;
        {error, ErrorID} ->
            {keep_state_and_data, [{reply, From, {error, ErrorID}}]}
    end;
action_state(EventType, EventContent, State, Data) ->
    all_state(EventType, EventContent, State, Data).

%% -----------------------------------------------------------------------
%% all state
%% -----------------------------------------------------------------------
all_state({call, From}, {enter_table, PlayerID}, State, Data) ->
    Reply = actor_action:enter(State, Data, PlayerID),
    {keep_state_and_data, [{reply, From, Reply}]};
all_state(cast, {leave_table, PlayerID}, _State, _Data) ->
    actor_action:leave(PlayerID),
    keep_state_and_data;
%% -----------------------------------------------------------------------
%% player_action
%% -----------------------------------------------------------------------
all_state({call, From}, {player_action, Action, Args}, State, Data) ->
    case erlang:apply(player_action, Action, [State, Data | Args]) of
        {ok, {Reply, NewData}} ->
            {keep_state, NewData, [{reply, From, Reply}]};
        {error, ErrorID} ->
            {keep_state_and_data, [{reply, From, {error, ErrorID}}]}
    end;
all_state({call, From}, _EventContent, _State, _Data) ->
    {keep_state_and_data, [{reply, From, {error, 1}}]};
all_state(EventType, EventContent, State, Data) ->
    ?DEBUG("EventType: ~p~n", [EventType]),
    ?DEBUG("EventContent: ~p~n", [EventContent]),
    ?DEBUG("State: ~p~n", [State]),
    ?DEBUG("Data: ~p~n", [Data]),
    keep_state_and_data.

%% @private
%% @doc If callback_mode is handle_event_function, then whenever a
%% gen_statem receives an event from call/2, cast/2, or as a normal
%% process message, this function is called.

handle_event(_EventType, _EventContent, _State, _Data) ->
    keep_state_and_data.

terminate(_Reason, _State, Data) ->
    #table_data{id = ID} = Data,
    table_mgr:unregister(ID),    
    ok.

%% @private
%% @doc Convert process state when code is changed
code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_next_round(State) ->
    case State of
        preflop ->
            flop;
        flop ->
            turn;
        turn ->
            river;
        river ->
            showdown
    end.
