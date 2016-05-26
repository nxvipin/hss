-module(hss_task_controller_sup).
-behaviour(supervisor).
-define(SERVER, ?MODULE).

-export([start_link/0,
         execute/2,
         init/1]).


start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

execute(Target, Script) ->
    Task = hss_task:new(Target, Script),
    {ok, TaskPID} = supervisor:start_child(?SERVER, [Task]),
    {ok, Task, TaskPID}.

init([]) ->

    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 1,
                 period => 5},

    TaskManagerSup = #{id => hss_task_manager_sup,
                start => {hss_task_manager_sup, start_link, []},
                restart => permanent,
                shutdown => 5000,
                type => supervisor,
                modules => [hss_task_manager_sup,
                            hss_task_manager]},

    {ok, {SupFlags, [TaskManagerSup]}}.
