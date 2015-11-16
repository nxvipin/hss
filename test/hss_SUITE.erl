-module(hss_SUITE).
-include_lib("common_test/include/ct.hrl").

%%% Test Cases
-export([basic_test/1]).

%%% Common test callbacks
-export([all/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2]).

-define(HSS_USER, "foo").
-define(HSS_PASSWORD, "bar").
-define(SCRIPT, "echo hello world").


%% -----------------------------------------------------------------------------
%% Common Test Interface Functions
%% -----------------------------------------------------------------------------


all() ->
    [basic_test].

init_per_suite(Config) ->
    catch crypto:stop(),
    case catch crypto:start() of
	ok ->
	    Config;
	_Else ->
	    {skip, "Crypto could not be started!"}
    end.

end_per_suite(_Config) ->
    ssh:stop(),
    crypto:stop().

init_per_testcase(_TestCase, Config) ->
    ssh:start(),
    hss:start(),
    SystemDir = ?config(data_dir, Config),
    DaemonOpts = [{system_dir, SystemDir},
                  {user_passwords, [{?HSS_USER, ?HSS_PASSWORD}]}],
    case hss_test_lib:daemon(DaemonOpts) of
        {_PID, Host, Port} ->
            [{host, Host}, {port, Port} | Config];
        _Error ->
            {skip, "SSH deamon could not be started"}
    end.

end_per_testcase(_TestCase, _Config) ->
    application:stop(hss),
    ssh:stop(),
    ok.

%% -----------------------------------------------------------------------------
%% Tests
%% -----------------------------------------------------------------------------

basic_test(Config) ->
    Host = proplists:get_value(host, Config),
    Port = proplists:get_value(port, Config),

    Machine = hss_machine:new(Host, Port),
    Credential = hss_credential:new(?HSS_USER, ?HSS_PASSWORD),
    {ok, _} = hss:run(Machine, Credential, ?SCRIPT).
