-module(hss_task_manager).
-behaviour(gen_server).
-include("hss.hrl").

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-record(state, {parent_pid :: pid(),
				task :: task(),
				machine_refs=[] :: list()}).


start_link(ParentPID, #task{task_id=TaskID}=Task) ->
	gen_server:start_link({global, {task_manager, TaskID}},
						  ?MODULE, [ParentPID, Task], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([ParentPID, Task]) ->
	self() ! monitor_machines,
	{ok, #state{parent_pid=ParentPID, task=Task, machine_refs=[]}}.

handle_call(_Request, _From, State) ->
	Reply = ok,
	{reply, Reply, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(monitor_machines, #state{parent_pid=ParentPID}=State) ->
	MachineManagerSup = get_machine_manager_sup_pid(ParentPID),
	Machines = supervisor:which_children(MachineManagerSup),
	MachinesPIDs = lists:filter(fun(M) -> is_pid(M) end,
								[M || {_, M, _, _} <- Machines]),
	lager:debug("Machine PID's: ~p", [MachinesPIDs]),
	MachineRefs = [erlang:monitor(process, MachinePID)
				   || MachinePID <- MachinesPIDs],
	{noreply, State#state{machine_refs=MachineRefs}};

handle_info({_, Ref, process, PID, _Reason},
			#state{parent_pid=ParentPID,
				   task=#task{task_id=TaskID},
				   machine_refs=MachineRefs0}=State) ->
	MachineRefs = lists:delete(Ref, MachineRefs0),
	lager:info("Machine Process Stop: ~p", [PID]),
	case MachineRefs of
		[] ->
			lager:notice("Task Completed: ~p. Terminating Task Subtree: ~p",
						 [TaskID, ParentPID]),
			supervisor:terminate_child(hss_sup, ParentPID);
		_ ->
			ok
	end,
	{noreply, State#state{machine_refs=MachineRefs}};

handle_info(Info, State) ->
	lager:notice("INFO: ~p", [Info]),
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

get_machine_manager_sup_pid(TaskManagerSupPid) ->
	Children = supervisor:which_children(TaskManagerSupPid),
	{ID, ChildPID, _, _} = lists:keyfind(hss_machine_manager_sup, 1, Children),
	ChildPID.
