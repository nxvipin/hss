-module(hss_task_manager_sup).
-behaviour(supervisor).
-include("hss.hrl").
-define(SERVER, ?MODULE).

-export([start_link/1,
         init/1]).

start_link(Task) ->
    supervisor:start_link(?MODULE, [Task]).

init([Task]) ->

    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 5},

    MachineManagerSup = #{id => hss_machine_manager_sup,
                          start => {hss_machine_manager_sup, start_link, [Task]},
                          restart => permanent,
                          shutdown => 5000,
                          type => supervisor,
                          modules => [hss_machine_manager_sup,
                                      hss_machine_manager]},

	TaskManager = #{id => hss_task_manager,
					start => {hss_task_manager, start_link, [self(), Task]},
					restart => permanent,
					shutdown => 5000,
					type => worker,
					modules => [hss_task_manager]},

    {ok, {SupFlags, [MachineManagerSup, TaskManager]}}.
