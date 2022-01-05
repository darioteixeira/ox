.DEFAULT_GOAL := build

.PHONY: build
build:
	dune build

.PHONY: clean
clean:
	dune clean
