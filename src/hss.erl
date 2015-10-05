-module(hss).
-compile(export_all).
-include("hss.hrl").


start() ->
    application:ensure_all_started(hss).


-spec run(#target{}, script()) -> hss_acceptor:run_results().
run(Target, Script) ->
    hss_acceptor:run(Target, Script).


-spec run(#machine{}, #credential{}, script()) -> hss_acceptor:run_result().
run(Machine, Credential, Script) ->
    hss_acceptor:run(Machine, Credential, Script).
