-module(hss_test_lib).
-export([daemon/1]).

daemon(Options) ->
    daemon(any, inet_port(), Options).

daemon(Host, Port, Options) ->
    ct:pal("SSH ->  HOST: ~p PORT: ~p OPTIONS: ~p~n", [Host, Port, Options]),
    case ssh:daemon(Host, Port, Options) of
        {ok, Pid} when Host == any ->
            {Pid, hostname(), Port};
        {ok, Pid} ->
            {Pid, Host, Port};
        Error ->
            Error
    end.

inet_port()->
    {ok, Socket} = gen_tcp:listen(0, [{reuseaddr, true}]),
    {ok, Port} = inet:port(Socket),
    gen_tcp:close(Socket),
    Port.

hostname() ->
    {ok,Host} = inet:gethostname(),
    Host.
