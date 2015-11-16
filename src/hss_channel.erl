-module(hss_channel).
-behaviour(ssh_channel).
-include("hss.hrl").
-define(SERVER, ?MODULE).
-compile(export_all).

-export([create/4, exec/2, exec/3]).

-record(cstate, {connection_pid :: connection_pid(),
                 channel_id :: channel_id(),
                 machine :: machine(),
                 task :: task(),
                 out_file_name,
                 out_file}).


%% -----------------------------------------------------------------------------
%% Public API
%% -----------------------------------------------------------------------------


create(ConnectionPID, ChannelID, Machine, Task) ->
    supervisor:start_child(hss_channel_sup,
                           [ConnectionPID, ChannelID, hss_channel,
                            [{connection_pid, ConnectionPID},
                             {channel_id, ChannelID},
                             {machine, Machine},
                             {task, Task}]
                           ]).

exec(Ref, Cmd) ->
    exec(Ref, Cmd, infinity).

exec(Ref, Cmd, Timeout) ->
    ssh_channel:cast(Ref, {exec, Cmd, Timeout}).


%% -----------------------------------------------------------------------------
%% Gen server callback
%% -----------------------------------------------------------------------------


init([{connection_pid, ConnectionPID}, {channel_id, ChannelID},
      {machine, Machine}, {task, Task}]) ->
    lager:info("~s Initialize Channel", [p(Task, Machine)]),
    CState = initialize_state(ConnectionPID, ChannelID, Machine, Task),
    {ok, CState}.

handle_call(Msg, _From, CState) ->
    {reply, Msg, CState}.

handle_cast({exec, Cmd, Timeout}, CState) ->
    lager:info("~s Execute Script", [p(CState)]),
    ssh_connection:exec(CState#cstate.connection_pid,
                        CState#cstate.channel_id,
                        Cmd, Timeout),
    {noreply, CState};

handle_cast(stop, CState) ->
    {stop, user_stop_channel, CState};

handle_cast(_Msg, CState) ->
    {noreply, CState}.

handle_msg(_Msg, CState)->
    {ok, CState}.

handle_ssh_msg({ssh_cm, ConnectionPID, {data, ChannelID, _DType, Data}},
               #cstate{connection_pid = ConnectionPID,
                       channel_id = ChannelID,
                       out_file = File} = CState) ->
    lager:debug("~s Data Received", [p(CState)]),
    file:write(File, Data),
    {ok, CState};

handle_ssh_msg({ssh_cm, ConnectionPID, {eof, ChannelID}},
               #cstate{connection_pid = ConnectionPID,
                       channel_id = ChannelID,
                       out_file = File} = CState) ->
    lager:info("~s EOF Received", [p(CState)]),
    file:close(File),
    {ok, CState};

handle_ssh_msg({ssh_cm, ConnectionPID, {exit_status, ChannelID, Status}},
               #cstate{connection_pid = ConnectionPID,
                       channel_id = ChannelID,
                       out_file = File} = CState) ->
    lager:info("~s Exit Status Received. Code: ~p", [p(CState), Status]),
    file:close(File),
    {ok, CState};

handle_ssh_msg({ssh_cm, ConnectionPID,
                {exit_signal, ChannelID, Signal, Error, Lang}},
               #cstate{connection_pid = ConnectionPID,
                       channel_id = ChannelID,
                       out_file = File} = CState) ->
    lager:info("~s Exit Singal Received. Signal: ~p, Error: ~p, Lang: ~p",
               [p(CState), Signal, Error, Lang]),
    file:close(File),
    {ok, CState}.

terminate(Reason, CState) ->
    lager:info("~s Terminate Channel. Reason: ~p", [p(CState), Reason]).

code_change(_OldVsn, _CState, _Extra) ->
    {ok, _CState}.


%% -----------------------------------------------------------------------------
%% Internal API
%% -----------------------------------------------------------------------------

initialize_state(ConnectionPID, ChannelID, Machine,
                 #task{task_id = TaskID} = Task) ->

    FileName = output_file_name(TaskID, Machine),
    {ok, File} = file:open(FileName, [write, raw, delayed_write]),

    lager:info("~s Output File: ~p", [p(Task, Machine), FileName]),

    #cstate{connection_pid = ConnectionPID,
            channel_id = ChannelID,
            machine = Machine,
            task = Task,
            out_file_name = FileName,
            out_file = File}.

output_file_name(TaskID, #machine{host = Host, port = Port}) ->
    DataDir = hss_utils:hss_data_directory(),
    TaskDir = filename:join(DataDir, TaskID),
    file:make_dir(TaskDir),
    FileName = Host ++ integer_to_list(Port),
    filename:join(TaskDir, FileName).

p(#cstate{task = Task, machine = Machine}) ->
    p(Task, Machine).

p(#task{task_id = TaskID}, #machine{host = Host, port = Port}) ->
    io_lib:format("[T:~s][M:~s:~p]", [TaskID, Host, Port]).
