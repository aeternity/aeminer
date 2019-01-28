REBAR = ./rebar3

.PHONY: all dialyzer test clean console

all:
	$(REBAR) compile

doc:
	$(REBAR) doc

dialyzer:
	$(REBAR) dialyzer

ct: all
	$(REBAR) ct test/aecuckoo_SUITE

clean:
	$(REBAR) clean
	rm -rf doc/*

distclean: clean
	rm -rf _build

console:
	$(REBAR) shell
