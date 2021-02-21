%%%-------------------------------------------------------------------
%% @doc logic public API
%% @end
%%%-------------------------------------------------------------------

-module(poker_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
	Reply = poker_sup:start_link(),
	web:start(),
	table:start(),
	main:start(),
	Reply.

stop(_State) ->
    ok.

%% internal functions
