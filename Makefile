.PHONY: test watch default top clean test_atd

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

test/%.atd: test
	$(EXEC) atdgen -j -j-std $@ -o -

test_atd: test/test.atd test/test2.atd test/readme.atd

gen:
    dune build @gen --auto-promote
