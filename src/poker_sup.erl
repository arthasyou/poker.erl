%%%-------------------------------------------------------------------
%% @doc logic top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(poker_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    recorder:create(),
    counter:create(),
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    TableSup = #{
        id => table_sup,
        start => {table_sup, start_link, []},
        restart => permanent,
        shutdown => infinity,
        type => supervisor,
        modules => [table_sup]
    },
    TableMgr = #{
        id => table_mgr,
        start => {table_mgr, start_link, []},
        restart => permanent,
        shutdown => infinity,
        type => worker,
        modules => [table_mgr]
    },

    ActorSup = #{
        id => actor_sup,
        start => {actor_sup, start_link, []},
        restart => permanent,
        shutdown => infinity,
        type => supervisor,
        modules => [actor_sup]
    },
    ActorMgr = #{
        id => actor_mgr,
        start => {actor_mgr, start_link, []},
        restart => permanent,
        shutdown => infinity,
        type => worker,
        modules => [actor_mgr]
    },

    ChildSpecs = [TableMgr, TableSup, ActorMgr, ActorSup],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
