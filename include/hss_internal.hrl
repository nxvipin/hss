-include("hss.hrl").

%% Machine State
-record(mstate, {task_id :: task_id(),
                 task :: task(),
                 target_machine :: machine(),
                 credential :: credential(),
                 connection_pid :: connection_pid(),
                 channel_id :: channel_id(),
                 channel_pid :: channel_pid()}).

-define(DEBUG(Context, Message),
		lager:debug(hss_utils:log(Context, Message))).
-define(DEBUG(Context, Message, Args),
		lager:debug(hss_utils:log(Context, Message), Args)).
-define(INFO(Context, Message),
		lager:info(hss_utils:log(Context, Message))).
-define(INFO(Context, Message, Args),
		lager:info(hss_utils:log(Context, Message), Args)).
-define(NOTICE(Context, Message),
		lager:notice(hss_utils:log(Context, Message))).
-define(NOTICE(Context, Message, Args),
		lager:notice(hss_utils:log(Context, Message), Args)).
-define(WARNING(Context, Message),
		lager:warning(hss_utils:log(Context, Message))).
-define(WARNING(Context, Message, Args),
		lager:warning(hss_utils:log(Context, Message), Args)).
-define(ERROR(Context, Message),
		lager:error(hss_utils:log(Context, Message))).
-define(ERROR(Context, Message, Args),
		lager:error(hss_utils:log(Context, Message), Args)).
