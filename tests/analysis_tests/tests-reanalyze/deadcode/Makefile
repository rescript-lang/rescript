SHELL = /bin/bash

build:
	yarn build

test: build node_modules/.bin/rescript
	./test.sh

# Order-independence test for reanalyze - not run by default (takes longer)
# Verifies that processing files in different orders produces identical results
test-reanalyze-order-independence: build node_modules/.bin/rescript
	./test-order-independence.sh

# Reactive server test - tests incremental analysis correctness
# Starts server, makes source changes, verifies delta matches expectation
test-reactive-server: build node_modules/.bin/rescript
	./test-reactive-server.sh --iterations 3

# Same test on the benchmark project (larger codebase)
test-reactive-server-benchmark:
	cd ../deadcode-benchmark && yarn build
	./test-reactive-server.sh --project ../deadcode-benchmark --iterations 1

# Benchmark: time analysis on existing compiled files
benchmark: build
	@echo "=== Benchmark on existing test files ==="
	@echo ""
	@echo "Sequential:"
	@time dune exec rescript-tools -- reanalyze -config -ci 2>/dev/null | grep "Analysis reported"

clean:
	yarn clean

.DEFAULT_GOAL := build

.PHONY: build clean test test-reanalyze-order-independence test-reactive-server test-reactive-server-benchmark benchmark
