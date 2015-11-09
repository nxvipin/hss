# HSS: SSH on Steroids! (WIP)
[![Build Status](https://travis-ci.org/swvist/hss.svg)](https://travis-ci.org/swvist/hss)


HSS is a SSH multiplexer available as an OTP application. It lets you run
scripts on targets. `Targets` have a list of one or more (unique) Machines that
share a common Credential. When a Script is run on a target, it gets run on
every machine associated with that Target.

##### Tests
make test

##### Quickstart
Compile the code and load all dependencies in path and drop into shell: `rebar3 shell`

In the erlang shell:

```erlang
hss:start().

Machine1 = hss_machine:new("machine1", 22).
Machine2 = hss_machine:new("machine2", 22).

Cred = hss_credential:new("username", "password").

Target = hss_target:new([Machine1, Machine2], Cred).

hss:run(Target, "ls /home").

```

### Feature Roadmap (In no specific order)

* Support passwordless & key based logins (Have a look at this [thread](http://erlang.org/pipermail/erlang-patches/2015-November/004800.html) and this [patch](https://github.com/erlang/otp/pull/884))
* ~~Reuse existing connections~~
* Write script output to file and/or publish it to subscribers
* Ability to specify various SSH timeouts


### Todo

* General code cleanup (Elvis?)
* Handle error cases
* Dialyzer Hints
* Request acceptor pool?
* Kill idle connections (after some timeout)
* Invalidate connection cache on connection close
* Run starts are serialized, but can be run in parallel.
