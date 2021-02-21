%%%-------------------------------------------------------------------
%%% @author luobin
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Feb 2021 12:57 PM
%%%-------------------------------------------------------------------
-module(river_action).
-author("luobin").

-include("table.hrl").

%% API
-export([start/1]).

start(Data) ->
    Data1 = table_deal_card:turn_or_river(Data),
    table_util:next_active_seat(Data1).
