-module(hss_channel).
-behaviour(ssh_channel).
-compile(export_all).

-record(channel_state, {connection, channel_id}).

init(Options) ->
    io:format("CHAN OPTS ~p ~n", [Options]),
    Connection = proplists:get_value(cm, Options),
    ChannelId = proplists:get_value(channel_id, Options),
    io:format("CHAN INIT ~p ~p ~n", [Connection, ChannelId]),
    {ok, #channel_state{connection=Connection,
                        channel_id=ChannelId}}.

handle_call(Msg, _From, State) ->
    io:format("CALL: ~p~n", [Msg]),
    {reply, Msg, State}.

handle_cast({exec, Cmd, Timeout}, State) ->
    ssh_connection:exec(State#channel_state.connection,
                        State#channel_state.channel_id,
                        Cmd, Timeout),
    io:format("CAST: ~p~n", [Cmd]),
    {noreply, State};

handle_cast(stop, State) ->
    {stop, user_stop_channel, State};

handle_cast(Msg, State) ->
    io:format("CAST: ~p~n", [Msg]),
    {noreply, State}.

handle_msg(Msg, State)->
    io:format("MESSAGE: ~p~n", [Msg]),
    {ok, State}.

handle_ssh_msg(Msg, State) ->
    io:format("SSHMESSAGE: ~p~n", [Msg]),
    {ok, State}.

terminate(Reason, State) ->
    io:format("TERMINATE: ~p : ~p~n", [Reason, State]).

code_change(_OldVsn, _State, _Extra) ->
    {ok, _State}.

exec(Ref, Cmd) ->
    exec(Ref, Cmd, infinity).

exec(Ref, Cmd, Timeout) ->
    ssh_channel:cast(Ref, {exec, Cmd, Timeout}).
