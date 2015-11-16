%%%-------------------------------------------------------------------
%%% @author Vipin Nair <swvist@gmail.com>
%%% @copyright (C) 2015, Vipin Nair
%%% @doc
%%%
%%% @end
%%% Created : 14 Sep 2015 by Vipin Nair <swvist@gmail.com>
%%%-------------------------------------------------------------------
-module(hss_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart intensity, and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->

    SupFlags =
        #{strategy => one_for_one,
          intensity => 1,
          period => 5},

    Acceptor =
        #{id => hss_acceptor,
          start => {hss_acceptor, start_link, []},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [hss_acceptor]},

    Connector =
        #{id => hss_connection,
          start => {hss_connection, start_link, []},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [hss_connection]},

    TaskControllerSup =
        #{id => hss_task_controller_sup,
          start => {hss_task_controller_sup, start_link, []},
          restart => permanent,
          shutdown => 5000,
          type => supervisor,
          modules => [hss_task_sup, hss_task]},

    {ok, {SupFlags, [Acceptor, Connector, TaskControllerSup]}}.
