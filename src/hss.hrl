
-type host() :: string().
-type ssh_port() :: integer().
-type username() :: string().
-type password() :: string().

-record(machine, {host :: host(),
                  port :: ssh_port()}).

-record(credential, {username :: username(),
                     password :: password()}).

-record(target, {machines = sets:new() :: sets:set(#machine{}),
                 credential :: #credential{}}).

-type connection_pid() :: pid().
-type channel_pid() :: pid().
-type channel_id() :: integer().
-type script() :: string().
