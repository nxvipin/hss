-module(hss_connection).
-include("hss.hrl").
-behaviour(gen_server).

-type connection_identifier() :: {host(), username()}.
-type connection_ref() :: pid() | undefined.
-type connection_cache() :: list({connection_identifier(), connection_ref()}).

-define(SERVER, ?MODULE).
-define(CONNECTION_TIMEOUT, 2000).
-define(NEGOTIATION_TIMEOUT, 2000).

%% API
-export([start_link/0, new/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-spec new(#machine{}, #credential{}) -> {ok, connection_ref()}.
new(Machine, Credential) ->
    gen_server:call(?SERVER, {connect, Machine, Credential}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    {ok, []}.

handle_call({connect, Machine, Credential}, _From, State) ->
    case create_connection(Machine, Credential, State) of
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

-spec create_connection(#machine{},
                        #credential{},
                        connection_cache()) ->
                               {ok, connection_ref(), connection_cache()} |
                               {error, string()}.

create_connection(Machine, Credential, State) ->
    Host = hss_machine:get_host(Machine),
    Port = hss_machine:get_port(Machine),
    Username = hss_credential:get_username(Credential),
    Password = hss_credential:get_password(Credential),

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
