%%%-------------------------------------------------------------------
%% @doc main top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(table_sup).

-behaviour(supervisor).

-export([start_link/0, start_child/1]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(Args) ->	
    supervisor:start_child(?SERVER, Args).


init([]) ->
    SupFlags = #{strategy => simple_one_for_one, intensity => 10, period => 10},

    Server = #{
        id => table_server,
        start => {table_server, start_link, []},
        restart => permanent,
        shutdown => infinity,
        type => worker,
        modules => [table_server]
    },

    ChildSpecs = [Server],

    {ok, {SupFlags, ChildSpecs}}.
%% internal functions
