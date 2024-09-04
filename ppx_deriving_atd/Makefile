include ../Makefile

.PHONY: test_atd test

test/%.atd: test
	$(EXEC) atdgen -j -j-std $@ -o -

test_atd: test/test.atd test/test2.atd test/readme.atd
