-module(hss_connection).
-behaviour(gen_server).

-type host() :: string().
-type ssh_port() :: integer().
-type username() :: string().
-type password() :: string().
-type connection_identifier() :: {host(), username()}.
-type connection_ref() :: pid() | undefined.
-type connection_cache() :: list({connection_identifier(), connection_ref()}).

%% API
-export([start_link/0, new/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(CONNECTION_TIMEOUT, 2000).
-define(NEGOTIATION_TIMEOUT, 2000).


-spec new(host(), ssh_port(), username(), password()) -> {ok, connection_ref()}.
new(Host, Port, Username, Password) ->
    gen_server:call(?SERVER, {connect, Host, Port, Username, Password}).


start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


init([]) ->
    {ok, []}.


handle_call({connect, Host, Port, Username, Password}, _From, State) ->
    case create_connection(Host, Port, Username, Password, State) of
        {ok, ConnRef, State_} ->
            {reply, {ok, ConnRef}, State_};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


-spec cache_get(host(), username(), connection_cache()) -> connection_ref().
cache_get(Host, Username, Cache) ->
    proplists:get_value({Host, Username}, Cache).


-spec cache_add(host(),
                username(),
                connection_ref(),
                connection_cache()) -> connection_cache().
cache_add(Host, Username, ConnRef, Cache) ->
    %% TODO: Should have only unique entries. Sets?
    Cache ++ [{{Host, Username}, ConnRef}].

-spec create_connection(host(),
                        ssh_port(),
                        username(),
                        password(),
                        connection_cache()) ->
                               {ok, connection_ref(), connection_cache()} |
                               {error, string()}.

create_connection(Host, Port, Username, Password, State) ->
    case cache_get(Host, Username, State) of
        undefined ->
            case ssh:connect(Host, Port,
                             [{user, Username},
                              {password, Password},
                              {connect_timeout, ?CONNECTION_TIMEOUT}],
                             ?NEGOTIATION_TIMEOUT) of
                {ok, ConnRef} ->
                    {ok, ConnRef, cache_add(Host, Username, ConnRef, State)};
                {error, Reason} ->
                    {error, Reason}
            end;
        ConnRef ->
            {ok, ConnRef, State}
    end.
