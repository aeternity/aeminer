REBAR = ./rebar3

.PHONY: all dialyzer ct eunit clean distclean console

all:
	$(REBAR) compile

doc:
	$(REBAR) doc

dialyzer:
	$(REBAR) dialyzer

ct: all
	$(REBAR) ct --suite=test/aecuckoo_SUITE

eunit:
	$(REBAR) eunit --module=aeminer_pow_tests,aeminer_pow_cuckoo_tests

clean:
	$(REBAR) clean
	rm -rf doc/*

distclean: clean
	rm -rf _build

console:
	$(REBAR) shell

