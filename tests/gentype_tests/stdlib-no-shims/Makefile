SHELL = /bin/bash

test:
	yarn workspaces focus @tests/gentype-stdlib-no-shims
	yarn build
	yarn typecheck

clean:
	yarn workspaces focus @tests/gentype-stdlib-no-shims
	yarn clean

.DEFAULT_GOAL := test

.PHONY: clean test
