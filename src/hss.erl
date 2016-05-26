-module(hss).
-export([start/0, run/2, run/3]).
-include("hss.hrl").

start() ->
    application:ensure_all_started(hss).

-spec run(target(), script()) -> {ok, task()}.
run(Target, Script) ->
    hss_task_controller_sup:execute(Target, Script).

-spec run(machine(), credential(), script()) -> {ok, task()}.
run(Machine, Credential, Script) ->
    Target = hss_target:new([Machine], Credential),
    run(Target, Script).
