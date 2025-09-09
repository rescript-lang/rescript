SHELL = /bin/bash

build:
	yarn workspaces focus @tests/gentype-genimport-single
	yarn build

test: build
	./test.sh

clean:
	yarn workspaces focus @tests/gentype-genimport-single
	yarn clean

.DEFAULT_GOAL := test

.PHONY: clean test
