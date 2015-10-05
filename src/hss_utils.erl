-module(hss_utils).
-export([default_timeout/0, default_ssh_timeout/0, default_conn_timeout/0,
         default_neg_timeout/0]).


default_timeout() ->
    default_ssh_timeout() + 5000.


default_ssh_timeout() ->
    default_conn_timeout() + default_neg_timeout().


default_conn_timeout() ->
    application:get_env(hss, default_ssh_connection_timeout, 20000).


default_neg_timeout() ->
    application:get_env(hss, default_ssh_negotiation_timeout, 10000).
