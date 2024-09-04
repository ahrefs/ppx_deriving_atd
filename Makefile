.PHONY: test watch default top clean

default:
	dune build

watch:
	dune build -w

test:
	dune build @runtest

top:
	dune utop

clean:
	dune clean
