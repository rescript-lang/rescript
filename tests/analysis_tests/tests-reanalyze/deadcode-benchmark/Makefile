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

# Benchmark with CMT cache
time-cache: generate build
	@echo "=== Without cache ==="
	@echo "Sequential:"
	@dune exec rescript-editor-analysis -- reanalyze -config -ci -timing 2>&1 | grep -E "=== Timing|CMT processing|File loading|Total:"
	@echo ""
	@echo "=== With CMT cache (first run - cold) ==="
	@echo "Sequential:"
	@dune exec rescript-editor-analysis -- reanalyze -config -ci -timing -cmt-cache 2>&1 | grep -E "=== Timing|CMT processing|File loading|Total:"
	@echo ""
	@echo "=== With CMT cache (second run - warm) ==="
	@echo "Sequential:"
	@dune exec rescript-editor-analysis -- reanalyze -config -ci -timing -cmt-cache 2>&1 | grep -E "=== Timing|CMT processing|File loading|Total:"
	@echo ""
	@echo "=== With CMT cache + parallel (warm) ==="
	@dune exec rescript-editor-analysis -- reanalyze -config -ci -timing -cmt-cache -parallel -1 2>&1 | grep -E "=== Timing|CMT processing|File loading|Total:"

# Benchmark reactive mode (simulates repeated analysis)
time-reactive: generate build
	@echo "=== Reactive mode benchmark ==="
	@echo ""
	@echo "Standard (baseline):"
	@dune exec rescript-editor-analysis -- reanalyze -config -ci -timing 2>&1 | grep -E "=== Timing|CMT processing|File loading|Total:"
	@echo ""
	@echo "Reactive mode (3 runs - first is cold, subsequent are warm):"
	@dune exec rescript-editor-analysis -- reanalyze -config -ci -timing -reactive -runs 3 2>/dev/null

.DEFAULT_GOAL := benchmark

.PHONY: generate build clean benchmark time time-cache time-reactive
