-module(hss_acceptor).
-behaviour(gen_server).
-include("hss.hrl").
-define(SERVER, ?MODULE).

-export([start_link/0, run/2, run/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export_type([run_result/0, run_results/0]).

-type run_results() :: list(run_result()).
-type run_result() :: {ok, connection_pid(), channel_id(), channel_pid()}
                    | {connection_error, term()}
                    | {channel_error, term()}.

%% -----------------------------------------------------------------------------
%% Public API
%% -----------------------------------------------------------------------------


start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


-spec run(#target{}, script()) -> run_results().
run(Target, Script) ->
    Machines = hss_target:get_machines(Target),
    Credential = hss_target:get_credential(Target),
    [run(Machine, Credential, Script) || Machine <- Machines].


-spec run(#machine{}, #credential{}, script()) -> run_result().
run(Machine, Credential, Script) ->
    gen_server:call(hss_acceptor, {exec, Machine, Credential, Script}).


%% -----------------------------------------------------------------------------
%% Gen server callback
%% -----------------------------------------------------------------------------


init([]) ->
    {ok, []}.

handle_call({exec, Machine, Credential, Command}, _From, _State) ->
    io:format("Exec: ~p~n", [Command]),

    Reply = case hss_connection:new(Machine, Credential) of
                {ok, Conn} ->
                    case ssh_connection:session_channel(Conn, 10000) of
                        {ok, ChannelId} ->
                            io:format("Created channel"),
                            {ok, Channel} =
                                supervisor:start_child(
                                  hss_channel_sup,
                                  [Conn, ChannelId, hss_channel,
                                   [{cm, Conn}, {channel_id, ChannelId}]
                                  ]),
                            hss_channel:exec(Channel, Command),
                            {ok, Conn, ChannelId, Channel};
                        {error, ChannelReason} ->
                            {channel_error, ChannelReason}
                    end;
                {error, Reason} ->
                    {connection_error, Reason}
            end,

    {reply, Reply, _State};

handle_call(_Request, _From, _State) ->
    Reply = ok,
    {reply, Reply, _State}.

handle_cast(_Msg, _State) ->
    {noreply, _State}.

handle_info(_Info, _State) ->
    {noreply, _State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, _State, _Extra) ->
    {ok, _State}.
