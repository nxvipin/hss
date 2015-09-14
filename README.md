# HSS: SSH on Steroids!

SSH multiplexer as an OTP application. Work in Progress.

##### Quickstart
Compile the code and load all dependencies in path and drop into shell: `rebar3 shell`

In the erlang shell:

```erlang
hss:start().
hss:run("<HOST>", <PORT>, "<USERNAME>", "<PASSWORD>", "<COMMAND>").
```

Example:
```erlang
hss:start().
hss:run("localhost", 22, "username", "password", "ls ~").
```


### Feature Roadmap

* Support passwordless & key based logins
* Reuse existing connections
* Write script output to file and/or publish it to subscribers


### Todo


* General code cleanup (Elvis?)
* Handle error cases
* Dialyzer Hints
* Request acceptor pool?
