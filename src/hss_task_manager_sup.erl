-module(hss_task_manager_sup).
-behaviour(supervisor).
-define(SERVER, ?MODULE).

-export([start_link/1,
         init/1]).


start_link(Task) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Task]).


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

    ChannelSup = #{id => hss_channel_sup,
                   start => {hss_channel_sup, start_link, []},
                   restart => permanent,
                   shutdown => 5000,
                   type => supervisor,
                   modules => [hss_channel_sup, hss_channel]},

    {ok, {SupFlags, [MachineManagerSup, ChannelSup]}}.
