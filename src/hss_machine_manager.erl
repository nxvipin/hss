-module(hss_machine_manager).
-behaviour(ssh_channel).

-include("hss_internal.hrl").

-define(SERVER, ?MODULE).

-export([create/2,
         start_link/2,
         start_link/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_msg/2,
         handle_ssh_msg/2,
         terminate/2,
         code_change/3]).



%% -----------------------------------------------------------------------------
%% Public API
%% -----------------------------------------------------------------------------


-spec create(machine(), task()) -> {ok, task()} | {error, any()}.
create(Machine, Task) ->
    supervisor:start_child(hss_machine_manager_sup, [Machine, Task]).

start_link(Machine, Task) ->
    {ok, _TaskPID} = gen_server:start_link(hss_machine_manager,
                                           [{machine, Machine}, {task, Task}], []).
start_link(MState) ->
    {ok, _TaskPID} = gen_server:start_link(hss_machine_manager,
                                           [MState], []).


%% -----------------------------------------------------------------------------
%% Gen server callback
%% -----------------------------------------------------------------------------

init([{machine, Machine}, {task, #task{task_id=TaskID}=Task}]) ->
    MState = initialize_state(Machine, Task),
    ?DEBUG(MState, "Initialize Machine"),
    handle_machine_start(self(), TaskID),
    {ok, MState};

init([#mstate{}=MState]) ->
    {ok, MState}.

handle_call(_Request, _From, _MState) ->
    Reply = ok,
    {reply, Reply, _MState}.

handle_cast({handle_machine_start, TaskID},
            #mstate{task_id=TaskID,
                    target_machine=Machine,
                    credential=Credential}=MState) ->
    ?DEBUG(MState, "Machine Start Request"),
    ConnRes = create_ssh_connection(Machine, Credential),
    handle_ssh_connection(self(), ConnRes),
    {noreply, MState};

handle_cast({handle_machine_start, _TaskID}, MState) ->
    ?ERROR(MState, "Machine Start Request, Task ID and Task does not match"),
    {noreply, MState};

handle_cast({handle_ssh_connection, {ok, ConnRef}}, MState) ->
    ?DEBUG(MState, "SSH connection created"),
    ChannelRes = create_ssh_channel(ConnRef),
    handle_ssh_channel(self(), ChannelRes),
    {noreply, MState#mstate{connection_pid=ConnRef}};

handle_cast({handle_ssh_connection, {error, Reason}}, MState) ->
    %% TODO: Retry logic sits here
    ?ERROR(MState, "SSH connection failed. Reason: ~p", [Reason]),
    {noreply, MState};

handle_cast({handle_ssh_channel, {ok, ChannelID}},
            #mstate{connection_pid=ConnPID,
                    task=#task{script=Script}}=MState) when is_pid(ConnPID) ->
    ?INFO(MState, "SSH channel created"),
    {ok, State}
        = ssh_channel:init([[{channel_cb, ?MODULE},
                             {init_args, [MState#mstate{channel_id=ChannelID}]},
                             {cm, ConnPID},
                             {channel_id, ChannelID}]]),
    handle_ssh_exec(self(), Script),
    ssh_channel:enter_loop(State);

handle_cast({handle_ssh_channel, {ok, _ChannelID}}, MState) ->
    ?ERROR(MState, "Invalid State; Channel created but connection not found"),
    {noreply, MState};

handle_cast({handle_ssh_channel, {error, Reason}}, MState) ->
    %% TODO: Retry logic sits here
    ?ERROR(MState, "SSH channel creation failed. Reason: ~p", [Reason]),
    {noreply, MState};

handle_cast({handle_ssh_exec, Script, Timeout},
            #mstate{connection_pid=ConnPID, channel_id=ChannelID}=MState) ->
    ?DEBUG(MState, "Starting Script Run"),
    ssh_connection:exec(ConnPID, ChannelID, Script, Timeout),
    {noreply, MState};

handle_cast(Msg, MState) ->
    ?INFO(MState, "Unknown Cast Message: ~p", [Msg]),
    {noreply, MState}.

handle_msg(_Info, _MState) ->
    {ok, _MState}.

handle_ssh_msg({ssh_cm, ConnectionPID, {data, ChannelID, _DType, _Data}},
               #mstate{connection_pid=ConnectionPID,
                       channel_id=ChannelID}=MState) ->
    ?DEBUG(MState, "Data Received"),
    {ok, MState};

handle_ssh_msg({ssh_cm, ConnectionPID, {eof, ChannelID}},
               #mstate{connection_pid=ConnectionPID,
                       channel_id=ChannelID}=MState) ->
    ?DEBUG(MState, "EOF Received"),
    {ok, MState};

handle_ssh_msg({ssh_cm, ConnectionPID, {exit_status, ChannelID, Status}},
               #mstate{connection_pid=ConnectionPID,
                       channel_id=ChannelID}=MState) ->
    ?DEBUG(MState, "Exit Status Received. Code: ~p", [Status]),
    {ok, MState};

handle_ssh_msg({ssh_cm, ConnectionPID,
                {exit_signal, ChannelID, Signal, Error, Lang}},
               #mstate{connection_pid=ConnectionPID,
                       channel_id=ChannelID}=MState) ->
    ?DEBUG(MState, "Exit Singal Received. Signal: ~p, Error: ~p, Lang: ~p",
           [Signal, Error, Lang]),
    {ok, MState}.

terminate(_Reason, _MState) ->
    ok.

code_change(_OldVsn, _MState, _Extra) ->
    {ok, _MState}.


%% -----------------------------------------------------------------------------
%% Internal API
%% -----------------------------------------------------------------------------

handle_machine_start(MachinePID, TaskID) ->
    ssh_channel:cast(MachinePID, {handle_machine_start, TaskID}).

handle_ssh_connection(MachinePID, ConnRes) ->
    ssh_channel:cast(MachinePID, {handle_ssh_connection, ConnRes}).

handle_ssh_channel(MachinePID, ChannelRes) ->
    ssh_channel:cast(MachinePID, {handle_ssh_channel, ChannelRes}).

handle_ssh_exec(MachinePID, Script) ->
    ssh_channel:cast(MachinePID, {handle_ssh_exec, Script, infinity}).

p(#mstate{task_id = TaskID, target_machine = #machine{host = Host,
                                                      port= Port}}) ->
    io_lib:format("[T:~s][M:~s:~p]", [TaskID, Host, Port]).

initialize_state(Machine,
                 #task{task_id = TaskID,
                       target = #target{credential = Credential}} = Task) ->
    #mstate{task_id = TaskID,
            task = Task,
            target_machine = Machine,
            credential = Credential}.

create_ssh_connection(Machine, Credential) ->
    %% TODO: Pattern Match
    %% TODO: Handle Credential Types
    Host = hss_machine:get_host(Machine),
    Port = hss_machine:get_port(Machine),
    Username = hss_credential:get_username(Credential),
    Password = hss_credential:get_password(Credential),

    case connection_cache_get(Host, Username) of
        undefined ->
            case ssh:connect(
                   Host, Port,
                   [{user, Username},
                    {password, Password},
                    {connect_timeout, hss_utils:default_conn_timeout()},
                    {silently_accept_hosts, hss_utils:accept_hosts()},
                    {user_dir, hss_utils:configure_user_dir(Credential)}],
                   hss_utils:default_neg_timeout()) of
                {ok, ConnRef} ->
                    connection_cache_add(Host, Username, ConnRef),
                    {ok, ConnRef};
                {error, Reason} ->
                    {error, Reason}
            end;
        ConnRef ->
            {ok, ConnRef}
    end.

connection_cache_get(Host, Username) ->
    global:whereis_name({conn, Host, Username}).

connection_cache_add(Host, Username, ConnRef) ->
    %% TODO: Handle registration failures.
    global:register_name({conn, Host, Username}, ConnRef).

create_ssh_channel(ConnectionPID) ->
    %% TODO: Channel timeout must be configurable
    ssh_connection:session_channel(ConnectionPID, 10000).
