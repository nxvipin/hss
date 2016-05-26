-module(hss_machine_manager_sup).
-include("hss.hrl").
-behaviour(supervisor).
-define(SERVER, ?MODULE).

-export([start_link/1,
         init/1]).

start_link(Task) ->
    supervisor:start_link(?MODULE, [Task]).

init([Task]) ->

    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 5},

    Machines = machine_spec(Task),

    {ok, {SupFlags, Machines}}.


machine_spec(#machine{}=Machine, #task{}=Task) ->
    #{id => hss_task_sup,
      start => {hss_machine_manager, start_link, [Machine, Task]},
      restart => temporary,
      shutdown => 5000,
      type => worker}.

machine_spec(#task{target=#target{machines=Machines}}=Task) ->
    [machine_spec(Machine, Task) || Machine <- Machines].
