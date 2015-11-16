-module(hss_task_controller_sup).
-behaviour(supervisor).
-define(SERVER, ?MODULE).

-export([start_link/0]).
-export([init/1]).


start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).


init([]) ->

    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 5},

    TaskManagerSup = #{id => hss_task_manager_sup,
                start => {hss_task_manager_sup, start_link, []},
                restart => permanent,
                shutdown => 5000,
                type => supervisor,
                modules => [hss_task_manager_sup,
                            hss_task_manager]},

    MachineManagerSup = #{id => hss_machine_manager_sup,
                          start => {hss_machine_manager_sup, start_link, []},
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

    {ok, {SupFlags, [TaskManagerSup, MachineManagerSup, ChannelSup]}}.
