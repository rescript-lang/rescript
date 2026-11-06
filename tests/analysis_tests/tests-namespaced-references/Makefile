SHELL = /bin/bash

build:
	yarn build

	# Simulate editor-written incremental CMTs
	mkdir -p lib/rescript/incremental
	cp lib/rescript/MyModule1-MyNamespace.cmt lib/rescript/incremental/MyModule1-MyNamespace.cmt
	cp lib/rescript/MyModule2-MyNamespace.cmt lib/rescript/incremental/MyModule2-MyNamespace.cmt
	cp lib/rescript/MyNamespace.cmt lib/rescript/incremental/MyNamespace.cmt

test: build
	bash ./test.sh

clean:
	yarn clean

.DEFAULT_GOAL := test

.PHONY: build clean test
