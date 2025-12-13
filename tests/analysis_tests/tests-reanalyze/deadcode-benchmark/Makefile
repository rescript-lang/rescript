SHELL = /bin/bash

# Number of copies to generate (default 50 = ~5000 files)
COPIES ?= 50

generate:
	chmod +x generate.sh
	./generate.sh $(COPIES)

node_modules/.bin/rescript:
	yarn install

build: node_modules/.bin/rescript
	yarn build

clean:
	rm -rf src lib node_modules

# Full benchmark: generate, build, then time analysis
benchmark: generate build
	@echo ""
	@echo "=== Benchmark: $(COPIES) copies (~$$(find src -name '*.res' | wc -l) files) ==="
	@echo ""
	@echo "Sequential:"
	@dune exec rescript-editor-analysis -- reanalyze -config -ci -timing 2>&1 | grep -E "Analysis reported|=== Timing|CMT processing|File loading|Result collection|Analysis:|Merging|Solving|Reporting:|Total:"
	@echo ""
	@echo "Parallel (auto-detect cores):"
	@dune exec rescript-editor-analysis -- reanalyze -config -ci -timing -parallel -1 2>&1 | grep -E "Analysis reported|=== Timing|CMT processing|File loading|Result collection|Analysis:|Merging|Solving|Reporting:|Total:"

# Just time analysis (assumes already built)
time:
	@echo "Sequential:"
	@dune exec rescript-editor-analysis -- reanalyze -config -ci -timing 2>&1 | grep -E "Analysis reported|=== Timing|CMT processing|File loading|Result collection|Analysis:|Merging|Solving|Reporting:|Total:"
	@echo ""
	@echo "Parallel (auto-detect cores):"
	@dune exec rescript-editor-analysis -- reanalyze -config -ci -timing -parallel -1 2>&1 | grep -E "Analysis reported|=== Timing|CMT processing|File loading|Result collection|Analysis:|Merging|Solving|Reporting:|Total:"

.DEFAULT_GOAL := benchmark

.PHONY: generate build clean benchmark time
