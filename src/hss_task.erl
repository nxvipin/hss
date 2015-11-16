-module(hss_task).
-include("hss.hrl").
-define(SERVER, ?MODULE).

-export([new/2, id/1]).

-spec new(target(), script()) -> task().
new(Target, Script) ->
    #task{task_id = hss_utils:uuid4str(),
          start_time = erlang:system_time(),
          target = Target,
          script = Script}.

-spec id(task()) -> task_id().
id(Task) ->
    Task#task.task_id.
