-module(hss).
-compile(export_all).

start() ->
    application:ensure_all_started(hss).

run(Target, Script) ->
    hss_acceptor:run(Target, Script).
