-module(hss_acceptor).
-behaviour(gen_server).
-include("hss.hrl").
-define(SERVER, ?MODULE).

-export([start_link/0, run/2, run/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).



%% -----------------------------------------------------------------------------
%% Public API
%% -----------------------------------------------------------------------------


start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


-spec run(#target{}, script()) -> {ok, task()}.
run(Target, Script) ->
    {ok, _Task} = gen_server:call(hss_acceptor, {exec, Target, Script},
                                  hss_utils:default_timeout()).

-spec run(#machine{}, #credential{}, script()) -> {ok, #task{}}.
run(Machine, Credential, Script) ->
    Target = hss_target:new([Machine], Credential),
    {ok, _Task} = run(Target, Script).


%% -----------------------------------------------------------------------------
%% Gen server callback
%% -----------------------------------------------------------------------------


init([]) ->
    {ok, []}.

handle_call({exec, Target, Script}, _From, _State) ->
    lager:info("[TASK:NEW] New Task Request: Target(~p) Script(~p)", [?R(Target), Script]),
    {ok, _TaskPID, Task} = hss_task_manager:execute(Target, Script),
    {reply, {ok, Task}, _State};

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
