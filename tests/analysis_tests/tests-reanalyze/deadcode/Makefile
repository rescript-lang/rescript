SHELL = /bin/bash

build:
	yarn build

test: build node_modules/.bin/rescript
	./test.sh

# Order-independence test for reanalyze - not run by default (takes longer)
# Verifies that processing files in different orders produces identical results
test-reanalyze-order-independence: build node_modules/.bin/rescript
	./test-order-independence.sh

# Parallel mode test - runs same tests but with parallel processing
test-parallel: build node_modules/.bin/rescript
	PARALLEL=4 ./test.sh

# Benchmark: time analysis on existing compiled files
benchmark: build
	@echo "=== Benchmark on existing test files ==="
	@echo ""
	@echo "Sequential:"
	@time dune exec rescript-editor-analysis -- reanalyze -config -ci 2>/dev/null | grep "Analysis reported"
	@echo ""
	@echo "Parallel (4 domains):"
	@time dune exec rescript-editor-analysis -- reanalyze -config -ci -parallel 4 2>/dev/null | grep "Analysis reported"

clean:
	yarn clean

.DEFAULT_GOAL := build

.PHONY: build clean test test-reanalyze-order-independence test-parallel benchmark
