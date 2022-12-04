.PHONY: run build
.DEFAULT_GOAL := run

build:
	@dune build

run:
	@dune exec aoc

