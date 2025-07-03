SHELL = /bin/bash

DUNE_BIN_DIR = ./_build/install/default/bin

build: ninja rewatch
	dune build
	./scripts/copyExes.js --compiler

watch:
	dune build -w

bench:
	$(DUNE_BIN_DIR)/syntax_benchmarks

dce:
	reanalyze.exe -dce-cmt _build/default/compiler

rewatch:
	cargo build --manifest-path rewatch/Cargo.toml --release
	./scripts/copyExes.js --rewatch

ninja/ninja:
	./scripts/buildNinjaBinary.js

ninja: ninja/ninja
	./scripts/copyExes.js --ninja

test: build lib
	node scripts/test.js -all

test-analysis:
	make -C tests/analysis_tests clean test

test-tools:
	make -C tests/tools_tests clean test

test-syntax:
	./scripts/test_syntax.sh

test-syntax-roundtrip:
	ROUNDTRIP_TEST=1 ./scripts/test_syntax.sh

test-gentype:
	make -C tests/gentype_tests/typescript-react-example clean test

test-rewatch:
	./rewatch/tests/suite-ci.sh

test-all: test test-gentype test-analysis test-tools test-rewatch

reanalyze:
	reanalyze.exe -set-exit-code -all-cmt _build/default/compiler _build/default/tests -exclude-paths compiler/outcome_printer,compiler/ml,compiler/frontend,compiler/ext,compiler/depends,compiler/core,compiler/common,compiler/cmij,compiler/bsb_helper,compiler/bsb

lib-bsb:
	./scripts/buildRuntimeLegacy.sh

lib:
	./scripts/buildRuntime.sh

artifacts: lib
	./scripts/npmPack.js --updateArtifactList

# Builds the core playground bundle (without the relevant cmijs files for the runtime)
playground:
	dune build --profile browser
	cp -f ./_build/default/compiler/jsoo/jsoo_playground_main.bc.js packages/playground/compiler.js

# Creates all the relevant core and third party cmij files to side-load together with the playground bundle
playground-cmijs: artifacts
	yarn workspace playground build

# Builds the playground, runs some e2e tests and releases the playground to the
# Cloudflare R2 (requires Rclone `rescript:` remote)
playground-release: playground playground-cmijs
	yarn workspace playground test
	yarn workspace playground upload-bundle

format:
	./scripts/format.sh

checkformat:
	./scripts/format_check.sh

clean-gentype:
	make -C tests/gentype_tests/typescript-react-example clean

clean-rewatch:
	cargo clean --manifest-path rewatch/Cargo.toml && rm -f rewatch/rewatch

clean:
	(cd runtime && ../cli/rescript.js clean)
	dune clean

clean-all: clean clean-gentype clean-rewatch

dev-container:
	docker build -t rescript-dev-container docker

.DEFAULT_GOAL := build

.PHONY: build watch rewatch ninja bench dce test test-syntax test-syntax-roundtrip test-gentype test-analysis test-tools test-all lib playground playground-cmijs playground-release artifacts format checkformat clean-gentype clean-rewatch clean clean-all dev-container
