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

build: compiler rewatch ninja

# Yarn

WORKSPACE_PACKAGE_JSONS := $(shell find packages -path '*/lib' -prune -o -name package.json -print)
YARN_INSTALL_SOURCES := package.json yarn.lock yarn.config.cjs .yarnrc.yml $(WORKSPACE_PACKAGE_JSONS)

yarn-install: $(YARN_INSTALL_STAMP)

$(YARN_INSTALL_STAMP): $(YARN_INSTALL_SOURCES)
	yarn install
	touch $@

# Ninja

NINJA_SOURCES = $(wildcard ninja/src/*.cc ninja/src/*.h) $(wildcard ninja/*.py)
NINJA_EXE = $(BIN_DIR)/ninja.exe

ninja: $(NINJA_EXE)

ninja/ninja: $(NINJA_SOURCES)
ifeq ($(OS),Darwin)
	export CXXFLAGS="-flto"
endif
	cd ninja && python3 configure.py --bootstrap --verbose

$(NINJA_EXE): ninja/ninja
	$(call COPY_EXE,$<,$@)

clean-ninja:
	rm -rf $(NINJA_EXE) ninja/build.ninja ninja/build ninja/misc/__pycache__ ninja/ninja

# Rewatch

REWATCH_SOURCES = $(shell find rewatch/src -name '*.rs') rewatch/Cargo.toml rewatch/Cargo.lock rewatch/rust-toolchain.toml
RESCRIPT_EXE = $(BIN_DIR)/rescript.exe

rewatch: $(RESCRIPT_EXE)

$(RESCRIPT_EXE): rewatch/target/debug/rescript$(PLATFORM_EXE_EXT)
	$(call COPY_EXE,$<,$@)

rewatch/target/debug/rescript$(PLATFORM_EXE_EXT): $(REWATCH_SOURCES)
	cargo build --manifest-path rewatch/Cargo.toml

clean-rewatch:
	cargo clean --manifest-path rewatch/Cargo.toml && rm -rf rewatch/target && rm -f $(RESCRIPT_EXE)

# Compiler

COMPILER_SOURCE_DIRS := compiler tests analysis tools
COMPILER_SOURCES = $(shell find $(COMPILER_SOURCE_DIRS) -type f \( -name '*.ml' -o -name '*.mli' -o -name '*.dune' -o -name dune -o -name dune-project \))
COMPILER_BIN_NAMES := bsc bsb_helper rescript-legacy rescript-editor-analysis rescript-tools
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

test: lib ninja
	node scripts/test.js -all

test-analysis: lib ninja
	make -C tests/analysis_tests clean test

test-tools: lib ninja
	make -C tests/tools_tests clean test

test-syntax: compiler
	./scripts/test_syntax.sh

test-syntax-roundtrip: compiler
	ROUNDTRIP_TEST=1 ./scripts/test_syntax.sh

test-gentype: lib ninja
	make -C tests/gentype_tests/typescript-react-example clean test
	make -C tests/gentype_tests/stdlib-no-shims clean test

test-rewatch: lib
	./rewatch/tests/suite.sh $(RESCRIPT_EXE)

test-all: test test-gentype test-analysis test-tools test-rewatch

# Builds the core playground bundle (without the relevant cmijs files for the runtime)
playground: | $(YARN_INSTALL_STAMP)
	dune build --profile browser
	cp -f ./_build/default/compiler/jsoo/jsoo_playground_main.bc.js packages/playground/compiler.js

# Creates all the relevant core and third party cmij files to side-load together with the playground bundle
playground-cmijs: | $(YARN_INSTALL_STAMP) # should also depend on artifacts, but that causes an attempt to copy binaries for JSOO
	yarn workspace playground build

# Builds the playground, runs some e2e tests and releases the playground to the
# Cloudflare R2 (requires Rclone `rescript:` remote)
playground-release: playground playground-cmijs | $(YARN_INSTALL_STAMP)
	yarn workspace playground test
	yarn workspace playground upload-bundle

format: | $(YARN_INSTALL_STAMP)
	./scripts/format.sh

checkformat: | $(YARN_INSTALL_STAMP)
	./scripts/format_check.sh

clean-gentype:
	make -C tests/gentype_tests/typescript-react-example clean
	make -C tests/gentype_tests/stdlib-no-shims clean

clean-tests: clean-gentype

clean: clean-lib clean-compiler clean-rewatch clean-ninja

dev-container:
	docker build -t rescript-dev-container docker

.DEFAULT_GOAL := build

.PHONY: yarn-install build ninja rewatch compiler lib artifacts bench test test-analysis test-tools test-syntax test-syntax-roundtrip test-gentype test-rewatch test-all playground playground-cmijs playground-release format checkformat clean-ninja clean-rewatch clean-compiler clean-lib clean-gentype clean-tests clean dev-container
