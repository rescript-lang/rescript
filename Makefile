SHELL = /bin/bash

DUNE_BIN_DIR = ./_build/install/default/bin

build: ninja
	dune build
	./scripts/copyExes.js

watch:
	dune build -w

bench:
	$(DUNE_BIN_DIR)/syntax_benchmarks

dce:
	reanalyze.exe -dce-cmt _build/default/jscomp

ninja/ninja:
	./scripts/buildNinjaBinary.js

ninja: ninja/ninja

node_modules/.bin/semver:
	npm install

test: lib
	node scripts/ciTest.js -all

test-syntax:
	bash ./scripts/test_syntax.sh
	make reanalyze
	bash ./scripts/testok.sh

test-syntax-roundtrip:
	ROUNDTRIP_TEST=1 bash ./scripts/test_syntax.sh
	make reanalyze
	bash ./scripts/testok.sh

test-gentype:
	make -C jscomp/gentype_tests/typescript-react-example clean test

test-all: test test-gentype

reanalyze:
	reanalyze.exe -set-exit-code -all-cmt _build/default/jscomp -suppress jscomp/syntax/testrunner -exclude-paths jscomp/outcome_printer,jscomp/ounit_tests,jscomp/ml,jscomp/js_parser,jscomp/frontend,jscomp/ext,jscomp/depends,jscomp/core,jscomp/common,jscomp/cmij,jscomp/bsb_helper,jscomp/bsb

lib: build node_modules/.bin/semver
	node scripts/ninja.js config
	node scripts/ninja.js build
	./scripts/prebuilt.js

artifacts: lib
	./scripts/npmPack.js -updateArtifactList

# Builds the core playground bundle (without the relevant cmijs files for the runtime)
playground:
	dune build --profile browser
	cp ./_build/default/jscomp/jsoo/jsoo_playground_main.bc.js playground/compiler.js

# Creates all the relevant core and third party cmij files to side-load together with the playground bundle
playground-cmijs: artifacts
	node packages/playground-bundling/scripts/generate_cmijs.js

# Builds the playground, runs some e2e tests and releases the playground to the
# CDN (requires KEYCDN_USER and KEYCDN_PASSWORD set in the env variables)
playground-release: playground playground-cmijs
	node playground/playground_test.js
	sh playground/upload_bundle.sh

format:
	dune build @fmt --auto-promote

checkformat:
	dune build @fmt

clean-gentype:
	make -C jscomp/gentype_tests/typescript-react-example clean

clean:
	dune clean
	./scripts/ninja.js clean

clean-all: clean clean-gentype

.DEFAULT_GOAL := build

.PHONY: build watch ninja bench dce test test-syntax test-syntax-roundtrip test-gentype test-all lib playground playground-cmijs playground-release artifacts format checkformat clean-gentype clean clean-all
