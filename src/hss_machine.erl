-module(hss_machine).
-include("hss.hrl").
-compile(export_all).

-spec new(host(), ssh_port()) -> #machine{}.
new(Host, Port) ->
    #machine{host = Host, port = Port}.

-spec get_host(#machine{}) -> host() | undefined.
get_host(Machine) ->
    Machine#machine.host.

-spec get_port(#machine{}) -> ssh_port() |  undefined.
get_port(Machine) ->
    Machine#machine.port.
