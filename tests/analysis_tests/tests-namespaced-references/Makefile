SHELL = /bin/bash

build:
	yarn build

	# Simulate editor-written incremental CMTs
	mkdir -p lib/bs/___incremental
	cp lib/bs/src/MyModule1-MyNamespace.cmt lib/bs/___incremental/MyModule1-MyNamespace.cmt
	cp lib/bs/src/MyModule2-MyNamespace.cmt lib/bs/___incremental/MyModule2-MyNamespace.cmt
	cp lib/bs/MyNamespace.cmt lib/bs/___incremental/MyNamespace.cmt

test: build
	./test.sh

clean:
	yarn clean

.DEFAULT_GOAL := test

.PHONY: build clean test
