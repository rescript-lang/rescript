SHELL = /bin/bash

test-analysis-binary:
	make -C tests test
	make -C tests-generic-jsx-transform test
	make -C tests-incremental-typechecking test

test-reanalyze:
	make -C tests-reanalyze test

test: test-analysis-binary test-reanalyze

clean:
	make -C tests clean
	make -C tests-generic-jsx-transform clean
	make -C tests-incremental-typechecking clean
	make -C tests-reanalyze clean

.PHONY: test-analysis-binary test-reanalyze clean test
