%%%-------------------------------------------------------------------
%%% @author Vipin Nair <swvist@gmail.com>
%%% @copyright (C) 2015, Vipin Nair
%%% @doc
%%%
%%% @end
%%% Created : 13 Sep 2015 by Vipin Nair <swvist@gmail.com>
%%%-------------------------------------------------------------------
-module(hss_channel_sup).

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
        #{strategy => simple_one_for_one,
          intensity => 1,
          period => 5},

    Channel =
        #{id => hss_channel,
          start => {ssh_channel, start_link, []},
          restart => temporary,
          shutdown => 5000,
          type => worker},

    {ok, {SupFlags, [Channel]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
