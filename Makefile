REBAR3_URL=https://s3.amazonaws.com/rebar3/rebar3

all: rebar3 clean test

rebar3:
	@echo "Rebar3 binary not found."
	test -f rebar3 || wget $(REBAR3_URL) && chmod +x rebar3

compile: rebar3
	./rebar3 compile

clean: rebar3
	./rebar3 clean

.PHONY: test
test: rebar3
	./rebar3 ct
