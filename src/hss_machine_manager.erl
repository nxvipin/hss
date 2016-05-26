-module(hss_machine_manager).
-behaviour(gen_server).

-include("hss_internal.hrl").

-define(SERVER, ?MODULE).

-export([create/2, start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).



%% -----------------------------------------------------------------------------
%% Public API
%% -----------------------------------------------------------------------------


-spec create(machine(), task()) -> {ok, task()} | {error, any()}.
create(Machine, Task) ->
    supervisor:start_child(hss_machine_manager_sup, [Machine, Task]).

start_link(Machine, Task) ->
    {ok, _TaskPID} = gen_server:start_link(hss_machine_manager,
                                           [{machine, Machine}, {task, Task}], []).


%% -----------------------------------------------------------------------------
%% Gen server callback
%% -----------------------------------------------------------------------------


init([{machine, Machine}, {task, #task{task_id=TaskID}=Task}]) ->
    MState = initialize_state(Machine, Task),
    ?DEBUG(MState, "Initialize Machine"),
    handle_machine_start(self(), TaskID),
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
    {noreply, MState#mstate{connection_pid = ConnRef}};

handle_cast({handle_ssh_connection, {error, Reason}}, MState) ->
    %% TODO: Retry logic sits here
    ?ERROR(MState, "SSH connection failed. Reason: ~p", [Reason]),
    {noreply, MState};

handle_cast({handle_ssh_channel, {ok, ChannelID}},
            #mstate{connection_pid=ConnPID,
                    target_machine=Machine,
                    task=Task}=MState) when is_pid(ConnPID) ->
    ?INFO(MState, "SSH channel created"),
    ChannelPRes = create_ssh_channel_process(ConnPID, ChannelID, Machine, Task),
    handle_ssh_channel_process(self(), ChannelPRes),
    {noreply, MState#mstate{channel_id=ChannelID}};

handle_cast({handle_ssh_channel, {ok, _ChannelID}}, MState) ->
    ?ERROR(MState, "Invalid State; Channel created but connection not found");

handle_cast({handle_ssh_channel, {error, Reason}}, MState) ->
    %% TODO: Retry logic sits here
    ?ERROR(MState, "SSH channel creation failed. Reason: ~p", [Reason]),
    {noreply, MState};

handle_cast({handle_ssh_channel_process, {ok, ChannelPID}},
            #mstate{connection_pid=ConnPID,
                    channel_id=ChannelID,
                    task=#task{script=Script}}=MState)
  when is_pid(ConnPID), is_integer(ChannelID) ->
    ?INFO(MState, "SSH channel handler process created"),
    hss_channel:exec(ChannelPID, Script),
    {noreply, MState#mstate{channel_pid=ChannelPID}};

handle_cast({handle_ssh_channel_process, {ok, _ChannelPID}}, MState) ->
    %% TODO: Update Task State
    lager:error("~s Invalid State. ConnectionPID or ChannelID incorrect", [p(MState)]),
    {noreply, MState};

handle_cast({handle_ssh_channel_process, {error, Reason}}, MState) ->
    %% TODO: Update Task State
    %% TODO: Retry logic sits here
    lager:error("~s SSH channel handler creation failed. Reason: ~p", [p(MState), Reason]),
    {noreply, MState};


handle_cast(_Msg, _MState) ->
    {noreply, _MState}.

handle_info(_Info, _MState) ->
    {noreply, _MState}.

terminate(_Reason, _MState) ->
    ok.

code_change(_OldVsn, _MState, _Extra) ->
    {ok, _MState}.


%% -----------------------------------------------------------------------------
%% Internal API
%% -----------------------------------------------------------------------------

handle_machine_start(MachinePID, TaskID) ->
    gen_server:cast(MachinePID, {handle_machine_start, TaskID}).

handle_ssh_connection(MachinePID, ConnRes) ->
    gen_server:cast(MachinePID, {handle_ssh_connection, ConnRes}).

handle_ssh_channel(MachinePID, ChannelRes) ->
    gen_server:cast(MachinePID, {handle_ssh_channel, ChannelRes}).

handle_ssh_channel_process(MachinePID, ChannelRes) ->
    gen_server:cast(MachinePID, {handle_ssh_channel_process, ChannelRes}).

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

create_ssh_channel_process(ConnectionPID, ChannelID, Machine, Task) ->
    hss_channel:create(ConnectionPID, ChannelID, Machine, Task).
