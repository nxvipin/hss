-module(hss_utils).
-export([default_timeout/0,
         default_ssh_timeout/0,
         default_conn_timeout/0,
         default_neg_timeout/0,
         accept_hosts/0,
         configure_user_dir/1,
         hss_data_directory/0,
         uuid4/0,
         uuid4str/0]).


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


hss_data_directory() ->
    case application:get_env(hss, hss_data_directory, default) of
        default ->
            {ok, [[HomeDir]]} = init:get_argument(home),
            DataDir = filename:join(HomeDir, ".hss"),
            file:make_dir(DataDir),
            DataDir;
        DataDir ->
            file:make_dir(DataDir),
            DataDir
    end.


uuid4() ->
    %% UUID4 implementation from gh/cloven/uuid4
    %% Credit: felixgallo@gmail.com
    RandomBytes = crypto:rand_bytes(16),
    <<First:32, Second:16, Third:12, Fourth:2,
      Fifth:12, Sixth:48, _UselessPadding:6, _Rest/binary>> = RandomBytes,
    Formatter = "~8.16.0b-~4.16.0b-4~3.16.0b-~1.16.0b~3.16.0b-~12.16.0b",
    list_to_binary(io_lib:format(Formatter, [First, Second, Third, Fourth+8,
                                             Fifth, Sixth])).


uuid4str() ->
    binary:bin_to_list(uuid4()).
