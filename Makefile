REBAR := ./rebar

.PHONY: all doc test clean release

all:
	$(REBAR) compile

doc:
	$(REBAR) doc

test:
	$(REBAR) eunit

clean:
	$(REBAR) clean

release: all test
	typer -r ./src/ > /dev/null
