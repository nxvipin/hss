-module(hss_task_manager_sup).
-behaviour(supervisor).
-define(SERVER, ?MODULE).

-export([start_link/0]).
-export([init/1]).


start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->

    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 1,
                 period => 5},

    TaskManager = #{id => hss_task_manager_sup,
                    start => {hss_task_manager, start_link, []},
                    restart => temporary,
                    shutdown => 5000,
                    type => worker},

    {ok, {SupFlags, [TaskManager]}}.
