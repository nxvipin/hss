-module(hss_target).
-include("hss.hrl").
-compile(export_all).

-spec new() -> #target{}.
new() ->
    #target{}.

-spec new(list(#machine{}), #credential{}) -> #target{}.
new(Machines, Credential) ->
    #target{machines = sets:from_list(Machines), credential = Credential}.

-spec add_machine(#machine{}, #target{}) -> #target{}.
add_machine(Machine, Target) ->
    Target#target{machines = sets:add_element(Machine, Target#target.machines)}.

-spec add_machines(list(#machine{}), #target{}) -> #target{}.
add_machines(Machines, Target) ->
    Target#target{machines = sets:union(sets:from_list(Machines),
                                        Target#target.machines)}.

-spec set_credential(#credential{}, #target{}) -> #target{}.
set_credential(Credential, Target) ->
    Target#target{credential = Credential}.

-spec get_machines(#target{}) -> list(#machine{}).
get_machines(Target) ->
    sets:to_list(Target#target.machines).

-spec get_credential(#target{}) -> #credential{}.
get_credential(Target) ->
    Target#target.credential.
