SHELL = /bin/bash

DUNE_BIN_DIR = ./_build/install/default/bin

build: ninja rewatch
	dune build
	./scripts/copyExes.js -compiler

watch:
	dune build -w

bench:
	$(DUNE_BIN_DIR)/syntax_benchmarks

dce:
	reanalyze.exe -dce-cmt _build/default/compiler

rewatch:
	cargo build --manifest-path rewatch/Cargo.toml
	cp rewatch/target/debug/rewatch rewatch
	./scripts/copyExes.js -rewatch

ninja/ninja:
	./scripts/buildNinjaBinary.js
	./scripts/copyExes.js -ninja

ninja: ninja/ninja

node_modules/.bin/semver:
	npm install

test: lib
	node scripts/test.js -all

test-analysis:
	make -C tests/analysis_tests clean test
	
test-tools:
	make -C tests/tools_tests clean test

test-syntax:
	bash ./scripts/test_syntax.sh
	bash ./scripts/testok.sh

test-syntax-roundtrip:
	ROUNDTRIP_TEST=1 bash ./scripts/test_syntax.sh
	bash ./scripts/testok.sh

test-gentype:
	make -C tests/gentype_tests/typescript-react-example clean test

test-all: test test-gentype test-analysis test-tools

reanalyze:
	reanalyze.exe -set-exit-code -all-cmt _build/default/compiler _build/default/tests -exclude-paths compiler/outcome_printer,compiler/ml,compiler/js_parser,compiler/frontend,compiler/ext,compiler/depends,compiler/core,compiler/common,compiler/cmij,compiler/bsb_helper,compiler/bsb

lib: build node_modules/.bin/semver
	./scripts/buildRuntime.sh
	./scripts/prebuilt.js

artifacts: lib
	./scripts/npmPack.js -updateArtifactList

# Builds the core playground bundle (without the relevant cmijs files for the runtime)
playground:
	dune build --profile browser
	cp ./_build/default/compiler/jsoo/jsoo_playground_main.bc.js playground/compiler.js

# Creates all the relevant core and third party cmij files to side-load together with the playground bundle
playground-cmijs: artifacts
	node packages/playground-bundling/scripts/generate_cmijs.js

# Builds the playground, runs some e2e tests and releases the playground to the
# CDN (requires KEYCDN_USER and KEYCDN_PASSWORD set in the env variables)
playground-release: playground playground-cmijs
	node playground/playground_test.js
	sh playground/upload_bundle.sh

format:
	bash scripts/format.sh

checkformat:
	bash scripts/format_check.sh

clean-gentype:
	make -C tests/gentype_tests/typescript-react-example clean

clean-rewatch:
	cargo clean --manifest-path rewatch/Cargo.toml && rm -f rewatch/rewatch

clean:
	(cd runtime && ../cli/rescript clean)
	dune clean

clean-all: clean clean-gentype clean-rewatch 

dev-container:
	docker build -t rescript-dev-container docker

.DEFAULT_GOAL := build

.PHONY: build watch rewatch ninja bench dce test test-syntax test-syntax-roundtrip test-gentype test-analysis test-tools test-all lib playground playground-cmijs playground-release artifacts format checkformat clean-gentype clean-rewatch clean clean-all dev-container
