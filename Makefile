REBAR=rebar3

all: compile

compile:
	$(REBAR) compile

run:
	$(REBAR) shell

.PHONY: test
test:
	rm -rf test/ct_logs
	$(REBAR) ct

.PHONY: clean
clean:
	rm -rf test/ct_logs
	$(REBAR) clean

