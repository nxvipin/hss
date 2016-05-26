-module(hss_task_manager).
-behaviour(gen_server).
-include("hss.hrl").
-define(SERVER, ?MODULE).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(task_state, {task_id :: task_id(),
                     task :: task()}).

%% -----------------------------------------------------------------------------
%% Public API
%% -----------------------------------------------------------------------------

start_link(Task) ->
    {ok, TaskPID} = gen_server:start_link(hss_task_manager, [Task], []),
    {ok, TaskPID, Task}.


%% -----------------------------------------------------------------------------
%% Gen server callback
%% -----------------------------------------------------------------------------


init([#task{task_id = TaskID} = Task]) ->
    lager:info("[T:~s] Initialize Task", [TaskID]),
    TaskState = initialize_state(Task),
    register_task(self(), TaskID),
    start_task(self(), TaskID),
    {ok, TaskState}.

handle_call(_Request, _From, _TaskState) ->
    Reply = ok,
    {reply, Reply, _TaskState}.

handle_cast({start, TaskID},
            #task_state{task_id = TaskID, task = Task} = TaskState) ->
    lager:info("[T:~s] Task Start Request", [TaskID]),
    create_machines(Task),
    {noreply, TaskState};

handle_cast({start, TaskID}, TaskState) ->
    lager:error("[T:~s] Task Start Request, Task ID and Task(~s) do not match",
                [TaskID, TaskState#task.task_id]),
    {noreply, TaskState};

handle_cast(_Msg, _TaskState) ->
    {noreply, _TaskState}.

handle_info(_Info, _TaskState) ->
    {noreply, _TaskState}.

terminate(_Reason, _TaskState) ->
    ok.

code_change(_OldVsn, _TaskState, _Extra) ->
    {ok, _TaskState}.


%% -----------------------------------------------------------------------------
%% Internal API
%% -----------------------------------------------------------------------------

initialize_state(#task{task_id = TaskID} = Task) ->
    #task_state{task_id = TaskID, task = Task}.

register_task(TaskPID, TaskID) ->
    %% TODO: Handle registration failure
    global:register_name({task_id, TaskID}, TaskPID).

start_task(TaskPID, TaskID) ->
    gen_server:cast(TaskPID, {start, TaskID}).

create_machines(#task{target = #target{machines = Machines}} = Task) ->
    [create_machine(Machine, Task) || Machine <- Machines].

create_machine(Machine, Task) ->
    hss_machine_manager:create(Machine, Task).
