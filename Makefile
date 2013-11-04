REBAR=`which rebar || echo ./rebar`

all: deps compile_deps

deps:
	@( $(REBAR) get-deps )

compile:
	@( $(REBAR) compile skip_deps=true )

compile_deps:
	@( $(REBAR) compile )

.PHONY: all deps compile compile_deps
