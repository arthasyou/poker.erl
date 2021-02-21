%%%-------------------------------------------------------------------
%%% @author luobin
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Nov 2020 3:31 PM
%%%-------------------------------------------------------------------
-module(actor_server).
-author("luobin").
-include("logger.hrl").
-include("actor.hrl").

-behaviour(gen_statem).

%% API
-export([start_link/1]).

%% gen_statem callbacks
-export([init/1, format_status/2, handle_event/4, terminate/3,
    code_change/4, callback_mode/0]).

%% state_name
-export([waiting/3]). 

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
    actor_mgr:register(ID, self()),
    {State, Data} = actor_init:init(ID),    
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
waiting({call, From}, {actor_ready, SeatID}, Data) ->
    ?DEBUG("waiting: ~p~n", [SeatID]),
    keep_state_and_data;
waiting(enter, _OldState, Data) ->
    ?DEBUG("waiting: ~p~n", [Data]),
    NewData = actor_callback:waiting(Data),
    {keep_state, NewData};
waiting(_EventType, _EventContent, Data) ->
    {keep_state, Data}.


%% @private
%% @doc If callback_mode is handle_event_function, then whenever a
%% gen_statem receives an event from call/2, cast/2, or as a normal
%% process message, this function is called.

handle_event(_EventType, _EventContent, _State, _Data) ->
    ?DEBUG("GOING HERE: ~p~n", [_EventContent]),
    keep_state_and_data.

terminate(_Reason, _State, Data) ->
    #actor_data{id = ID} = Data,
    actor_mgr:unregister(ID),    
    ok.

%% @private
%% @doc Convert process state when code is changed
code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
