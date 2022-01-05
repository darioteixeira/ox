.DEFAULT_GOAL := build

.PHONY: build
build:
	dune build

.PHONY: doc
doc:
	dune build @doc

.PHONY: clean
clean:
	dune clean
