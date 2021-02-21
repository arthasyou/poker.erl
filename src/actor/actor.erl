-module(actor).

%%%===================================================================
%%% API
%%%===================================================================

-export([start/0, spawn_actor/0]).

start() ->
    counter:init(actor_id).

spawn_actor() ->
    ID = counter:up(actor_id),
    table_sup:start_child([ID]).