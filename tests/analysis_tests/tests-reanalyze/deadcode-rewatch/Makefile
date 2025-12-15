SHELL = /bin/bash

build:
	yarn build

test: build node_modules/.bin/rescript
	./test.sh

# Order-independence test for reanalyze - not run by default (takes longer)
# Verifies that processing files in different orders produces identical results
test-reanalyze-order-independence: build node_modules/.bin/rescript
	./test-order-independence.sh

clean:
	yarn clean

.DEFAULT_GOAL := build

.PHONY: build clean test test-reanalyze-order-independence
