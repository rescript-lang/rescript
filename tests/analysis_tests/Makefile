SHELL = /bin/bash

test-analysis-binary:
	make -C tests test
	make -C tests-generic-jsx-transform test
	make -C tests-incremental-typechecking test
	make -C tests-namespaced-references test
	make -C tests-sourcedirs-dependency test

# Belt is shared by CommonJS and ES module test projects. Rebuild it until
# Rewatch invalidates source dependencies when package output settings change:
# https://github.com/rescript-lang/rescript/issues/8539
test-reanalyze:
	yarn workspace @rescript/belt rescript clean
	yarn workspace @rescript/belt build
	make -C tests-reanalyze test

test: test-analysis-binary test-reanalyze

clean:
	make -C tests clean
	make -C tests-generic-jsx-transform clean
	make -C tests-incremental-typechecking clean
	make -C tests-namespaced-references clean
	make -C tests-sourcedirs-dependency clean
	make -C tests-reanalyze clean

.PHONY: test-analysis-binary test-reanalyze clean test
