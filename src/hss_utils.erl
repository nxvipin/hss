-module(hss_utils).
-export([default_timeout/0, default_ssh_timeout/0, default_conn_timeout/0,
         default_neg_timeout/0, accept_hosts/0, configure_user_dir/1]).


default_timeout() ->
    default_ssh_timeout() + 5000.


default_ssh_timeout() ->
    default_conn_timeout() + default_neg_timeout().


default_conn_timeout() ->
    application:get_env(hss, default_ssh_connection_timeout, 20000).


default_neg_timeout() ->
    application:get_env(hss, default_ssh_negotiation_timeout, 10000).


accept_hosts() ->
    application:get_env(hss, silently_accept_hosts, true).


configure_user_dir(_Credential) ->
    %% TODO: We do not support key based logins yet. We create an empty userdir
    %% and pass it along to prevent key based login attempts. This directory is
    %% assumed to be empty.
    UserDir = "/tmp/hss",
    file:make_dir(UserDir),
    UserDir.
