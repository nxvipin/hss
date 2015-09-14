-module(hss).
-compile(export_all).

start() ->
    application:ensure_all_started(hss).

run(Host, Port, Username, Password, Script) ->
    gen_server:cast(hss_acceptor, {exec,
                                   {Host, Port},
                                   {Username, Password},
                                   Script}).
