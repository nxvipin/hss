-module(hss_acceptor).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-define(SERVER, ?MODULE).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


init([]) ->
    {ok, []}.

handle_call({exec, Machine, Credential, Command}, _From, _State) ->
    io:format("Exec: ~p~n", [Command]),

    {Host, Port} = Machine,
    {Username, Password} = Credential,

    Reply = case hss_connection:new(Host, Port, Username, Password) of
                {ok, Conn} ->
                    case ssh_connection:session_channel(Conn, 10000) of
                        {ok, ChannelId} ->
                            {ok, Channel} =
                                supervisor:start_child(hss_channel_sup,
                                                       [Conn,
                                                        ChannelId,
                                                        hss_channel,
                                                        [{cm, Conn},
                                                         {channel_id, ChannelId}]
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
