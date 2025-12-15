SHELL = /bin/bash

build:
	make -C deadcode build
	make -C deadcode-rewatch build
	make -C termination build

test:
	make -C deadcode test
	make -C deadcode-rewatch test
	make -C termination test

# Order-independence test for reanalyze - not run by default (takes longer)
# Verifies that processing files in different orders produces identical results
test-reanalyze-order-independence:
	make -C deadcode test-reanalyze-order-independence

clean:
	make -C deadcode clean
	make -C termination clean

.DEFAULT_GOAL := build

.PHONY: build clean test test-reanalyze-order-independence
