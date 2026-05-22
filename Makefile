SHELL = /bin/bash

ifeq ($(OS),Windows_NT)
	PLATFORM_EXE_EXT = .exe
else
	PLATFORM_EXE_EXT =
endif

ifneq ($(OS),Windows_NT)
	UNAME_S := $(shell uname -s)
	UNAME_M := $(shell uname -m)
endif

ifeq ($(OS),Windows_NT)
	RESCRIPT_PLATFORM := win32-x64
else ifeq ($(UNAME_S),Darwin)
	ifeq ($(UNAME_M),arm64)
		RESCRIPT_PLATFORM := darwin-arm64
	else
		RESCRIPT_PLATFORM := darwin-x64
	endif
else ifeq ($(UNAME_S),Linux)
	ifeq ($(UNAME_M),aarch64)
		RESCRIPT_PLATFORM := linux-arm64
	else ifeq ($(UNAME_M),arm64)
		RESCRIPT_PLATFORM := linux-arm64
	else
		RESCRIPT_PLATFORM := linux-x64
	endif
else
	$(error Unsupported platform $(UNAME_S)-$(UNAME_M))
endif

define COPY_EXE
	cp $1 $2
	chmod 755 $2
$(if $(filter Windows_NT,$(OS)),,strip $2)
endef

# Directories

BIN_DIR := packages/@rescript/$(RESCRIPT_PLATFORM)/bin
RUNTIME_DIR := packages/@rescript/runtime
DUNE_BIN_DIR = ./_build/install/default/bin

# Build stamps

# Yarn creates `.yarn/install-state.gz` whenever dependencies are installed.
# Using that file as our stamp ensures manual `yarn install` runs are detected.
YARN_INSTALL_STAMP := .yarn/install-state.gz
# Dune updates `_build/log` for every build invocation, even when run manually.
# Treat that log file as the compiler build stamp so manual `dune build`
# keeps Make targets up to date.
COMPILER_BUILD_STAMP := _build/log
# Runtime workspace touches this stamp (packages/@rescript/runtime/.buildstamp)
# after running `yarn workspace @rescript/runtime build`, which now runs `touch`
# as part of its build script.
RUNTIME_BUILD_STAMP := packages/@rescript/runtime/.buildstamp

# Default target

build: compiler rewatch

# Yarn

WORKSPACE_PACKAGE_JSONS := $(shell find packages -path '*/lib' -prune -o -name package.json -print)
YARN_INSTALL_SOURCES := package.json yarn.lock yarn.config.cjs .yarnrc.yml $(WORKSPACE_PACKAGE_JSONS)

yarn-install: $(YARN_INSTALL_STAMP)

$(YARN_INSTALL_STAMP): $(YARN_INSTALL_SOURCES)
	yarn install
	touch $@

# Rewatch

REWATCH_SOURCES = $(shell find rewatch/src -name '*.rs') rewatch/Cargo.toml rewatch/Cargo.lock rewatch/rust-toolchain.toml
RESCRIPT_EXE = $(BIN_DIR)/rescript.exe
ifdef CI
	REWATCH_PROFILE := release
	REWATCH_CARGO_FLAGS := --release
else
	REWATCH_PROFILE := debug
	REWATCH_CARGO_FLAGS :=
endif
REWATCH_TARGET := rewatch/target/$(REWATCH_PROFILE)/rescript$(PLATFORM_EXE_EXT)

rewatch: $(RESCRIPT_EXE)

$(RESCRIPT_EXE): $(REWATCH_TARGET)
	$(call COPY_EXE,$<,$@)

$(REWATCH_TARGET): $(REWATCH_SOURCES)
	cargo build --manifest-path rewatch/Cargo.toml $(REWATCH_CARGO_FLAGS)

clean-rewatch:
	cargo clean --manifest-path rewatch/Cargo.toml && rm -rf rewatch/target && rm -f $(RESCRIPT_EXE)

# Compiler

COMPILER_SOURCE_DIRS := compiler tests analysis tools
COMPILER_SOURCES = $(shell find $(COMPILER_SOURCE_DIRS) -type f \( -name '*.ml' -o -name '*.mli' -o -name '*.dune' -o -name dune -o -name dune-project \))
COMPILER_BIN_NAMES := bsc rescript-editor-analysis rescript-tools
COMPILER_EXES := $(addsuffix .exe,$(addprefix $(BIN_DIR)/,$(COMPILER_BIN_NAMES)))
COMPILER_DUNE_BINS := $(addsuffix $(PLATFORM_EXE_EXT),$(addprefix $(DUNE_BIN_DIR)/,$(COMPILER_BIN_NAMES)))

compiler: $(COMPILER_EXES)

define MAKE_COMPILER_COPY_RULE
$(BIN_DIR)/$(1).exe: $(DUNE_BIN_DIR)/$(1)$(PLATFORM_EXE_EXT)
	$$(call COPY_EXE,$$<,$$@)
endef

$(foreach bin,$(COMPILER_BIN_NAMES),$(eval $(call MAKE_COMPILER_COPY_RULE,$(bin))))

# "touch" after dune build to make sure that the binaries' timestamps are updated
# even if the actual content of the sources hasn't changed.
$(COMPILER_BUILD_STAMP): $(COMPILER_SOURCES)
	dune build
	@$(foreach bin,$(COMPILER_DUNE_BINS),touch $(bin);)

$(COMPILER_DUNE_BINS): $(COMPILER_BUILD_STAMP) ;

clean-compiler:
	dune clean && rm -f $(COMPILER_EXES) $(COMPILER_BUILD_STAMP)

# Runtime / stdlib

RUNTIME_SOURCES := $(shell find $(RUNTIME_DIR) -path '$(RUNTIME_DIR)/lib' -prune -o -type f \( -name '*.res' -o -name '*.resi' -o -name 'rescript.json' \) -print)

lib: $(RUNTIME_BUILD_STAMP)

$(RUNTIME_BUILD_STAMP): $(RUNTIME_SOURCES) $(COMPILER_EXES) $(RESCRIPT_EXE) | $(YARN_INSTALL_STAMP)
	yarn workspace @rescript/runtime build

clean-lib:
	yarn workspace @rescript/runtime rescript clean
	rm -f $(RUNTIME_BUILD_STAMP)

# Artifact list

artifacts: lib
	./scripts/updateArtifactList.js

# Tests

bench: compiler
	$(DUNE_BIN_DIR)/syntax_benchmarks

test: lib
	node scripts/test.js -all

test-analysis: lib
	make -C tests/analysis_tests clean test

test-reanalyze: lib
	make -C tests/analysis_tests/tests-reanalyze/deadcode test

# Benchmark reanalyze on larger codebase (COPIES=N for more files)
benchmark-reanalyze: lib
	make -C tests/analysis_tests/tests-reanalyze/deadcode-benchmark benchmark COPIES=$(or $(COPIES),50)

test-tools: lib
	make -C tests/tools_tests clean test

test-syntax: compiler
	./scripts/test_syntax.sh

test-syntax-roundtrip: compiler
	ROUNDTRIP_TEST=1 ./scripts/test_syntax.sh

test-gentype: lib
	make -C tests/gentype_tests/typescript-react-example clean test
	make -C tests/gentype_tests/stdlib-no-shims clean test

test-rewatch: lib
	./rewatch/tests/suite.sh $(RESCRIPT_EXE)

test-all: test test-gentype test-analysis test-tools test-rewatch

# Playground

PLAYGROUND_BUILD_DIR := ./_build_playground
PLAYGROUND_BUILD_STAMP := $(PLAYGROUND_BUILD_DIR)/log # touched by dune on each build
PLAYGROUND_COMPILER := packages/playground/compiler.js
PLAYGROUND_CMI_BUILD_STAMP := packages/playground/.buildstamp # touched by playground npm build script

playground: playground-compiler playground-cmijs

playground-compiler: $(PLAYGROUND_COMPILER)

$(PLAYGROUND_COMPILER): $(PLAYGROUND_BUILD_STAMP)

$(PLAYGROUND_BUILD_STAMP): $(COMPILER_SOURCES)
	dune build --profile browser --build-dir $(PLAYGROUND_BUILD_DIR)
	cp -f $(PLAYGROUND_BUILD_DIR)/default/compiler/jsoo/jsoo_playground_main.bc.js $(PLAYGROUND_COMPILER)

# Creates all the relevant core and third party cmij files to side-load together with the playground bundle
playground-cmijs: $(PLAYGROUND_CMI_BUILD_STAMP)

$(PLAYGROUND_CMI_BUILD_STAMP): $(RUNTIME_BUILD_STAMP)
	yarn workspace playground build

playground-test: playground
	yarn workspace playground test

# Builds the playground, runs some e2e tests and releases the playground to the
# Cloudflare R2 (requires Rclone `rescript:` remote)
playground-release: playground-test
	yarn workspace playground upload-bundle

# Format

format: | $(YARN_INSTALL_STAMP)
	./scripts/format.sh

checkformat: | $(YARN_INSTALL_STAMP)
	./scripts/format_check.sh

# Coverage (bisect_ppx)
#
# Requires the `bisect_ppx` opam package (>= 2.8.0) in your switch:
#   opam install bisect_ppx
# or pull it in via the rescript dev-setup deps:
#   opam install . --deps-only --with-dev-setup
#
# Quick start:
#   make coverage         # run full test suite, generate report
#   make clean-coverage   # remove coverage artifacts
#
# Outputs (under _coverage/):
#   html/index.html  — human-browsable line-level report
#   coverage.json    — Coveralls-format JSON, queryable with jq:
#                      { source_files: [{ name, coverage: [null|N, ...] }] }
#                      null = not instrumented, 0 = uncovered, N > 0 = hit count
#                      e.g. uncovered line numbers in one file:
#                      jq -r --arg f compiler/ml/typecore.ml \
#                        '.source_files[] | select(.name==$f) | .coverage
#                         | to_entries[] | select(.value==0) | (.key+1)' \
#                        _coverage/coverage.json

COVERAGE_DIR := _coverage
COVERAGE_FILES_DIR := $(COVERAGE_DIR)/files
COVERAGE_HTML_DIR := $(COVERAGE_DIR)/html
COVERAGE_JSON := $(COVERAGE_DIR)/coverage.json
COVERAGE_BISECT_PREFIX := $(abspath $(COVERAGE_FILES_DIR))/bisect

# Re-builds the toolchain with bisect_ppx instrumentation and swaps the
# instrumented binaries into BIN_DIR so any test runner that shells out to
# `bsc` produces .coverage files.
.PHONY: coverage-build
coverage-build: | $(YARN_INSTALL_STAMP)
	dune build --instrument-with bisect_ppx
	@$(foreach bin,$(COMPILER_DUNE_BINS),touch $(bin);)
	@$(foreach bin,$(COMPILER_BIN_NAMES), \
		cp $(DUNE_BIN_DIR)/$(bin)$(PLATFORM_EXE_EXT) $(BIN_DIR)/$(bin).exe && \
		chmod 755 $(BIN_DIR)/$(bin).exe;)

.PHONY: coverage-prepare
coverage-prepare: clean-coverage coverage-build
	mkdir -p $(COVERAGE_FILES_DIR)

# Build the runtime with the instrumented bsc so subsequent test runs have
# a fresh stdlib. Coverage from the runtime build is discarded so reports
# only reflect what the tests exercised.
.PHONY: coverage-lib
coverage-lib: coverage-prepare
	BISECT_FILE=$(COVERAGE_BISECT_PREFIX)-discard BISECT_SILENT=YES \
		yarn workspace @rescript/runtime build
	rm -f $(COVERAGE_BISECT_PREFIX)-discard*.coverage

.PHONY: coverage-run
coverage-run: coverage-lib
	BISECT_FILE=$(COVERAGE_BISECT_PREFIX) BISECT_SILENT=YES \
		node scripts/test.js -all

.PHONY: coverage-report
coverage-report:
	bisect-ppx-report html \
		--coverage-path $(COVERAGE_FILES_DIR) \
		--ignore-missing-files \
		-o $(COVERAGE_HTML_DIR)
	bisect-ppx-report coveralls \
		--coverage-path $(COVERAGE_FILES_DIR) \
		--ignore-missing-files \
		$(COVERAGE_JSON)
	bisect-ppx-report summary \
		--coverage-path $(COVERAGE_FILES_DIR)
	@echo ""
	@echo "HTML report: $(COVERAGE_HTML_DIR)/index.html"
	@echo "JSON data:   $(COVERAGE_JSON)"

# Drop instrumented binaries and the compiler build stamp so the next
# `make` / `make test` rebuilds uninstrumented toolchain artifacts
# instead of silently reusing the bisect_ppx-instrumented ones left in
# BIN_DIR by `coverage-build`.
.PHONY: coverage
coverage: coverage-run coverage-report
	rm -f $(COMPILER_EXES) $(COMPILER_BUILD_STAMP)

.PHONY: clean-coverage
clean-coverage:
	rm -rf $(COVERAGE_DIR)
	find . -name 'bisect*.coverage' -not -path './_build/*' -delete

# Clean

clean-gentype:
	make -C tests/gentype_tests/typescript-react-example clean
	make -C tests/gentype_tests/stdlib-no-shims clean

clean-tests: clean-gentype

clean: clean-lib clean-compiler clean-rewatch clean-coverage

dev-container:
	docker build -t rescript-dev-container docker

.DEFAULT_GOAL := build

.PHONY: yarn-install build rewatch compiler lib artifacts bench test test-analysis test-reanalyze benchmark-reanalyze test-tools test-syntax test-syntax-roundtrip test-gentype test-rewatch test-all playground playground-compiler playground-test playground-cmijs playground-release format checkformat clean-rewatch clean-compiler clean-lib clean-gentype clean-tests clean dev-container
