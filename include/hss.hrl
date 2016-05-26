-define(R(X), lager:pr(X, ?MODULE)).

-type host() :: string().
-type ssh_port() :: integer().
-type username() :: string().
-type password() :: string().
-type connection_pid() :: pid().
-type channel_pid() :: pid().
-type channel_id() :: integer().
-type script() :: string().
-type task_id() :: string().
-type task_pid() :: pid().

-record(machine, {host :: host(),
                  port :: ssh_port()}).

-record(credential, {username :: username(),
                     password :: password()}).

-record(target, {machines = [] :: list(machine()),
                 credential :: credential()}).

-record(task, {task_id :: task_id(),
               start_time :: integer(),
               end_time :: integer() | undefined,
               target :: target(),
               script :: script()}).

-type machine() :: #machine{}.
-type credential() :: #credential{}.
-type target() :: #target{}.
-type task() :: #task{}.
