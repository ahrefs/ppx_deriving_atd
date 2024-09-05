.PHONY: test watch default top clean test_atd test

default:
	dune build ./src

watch:
	dune build ./src -w

test:
	dune build ./test @runtest

top:
	dune utop ./src ppx_deriving_atd

clean:
	dune clean

test/%.atd: test
	$(EXEC) atdgen -j -j-std $@ -o -

test_atd: test/test.atd test/test2.atd test/readme.atd
