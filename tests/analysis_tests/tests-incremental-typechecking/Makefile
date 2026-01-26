SHELL = /bin/bash

build:
	yarn build > /dev/null 2>&1 || true

test: build
	./test.sh

clean:
	yarn clean

.DEFAULT_GOAL := test

.PHONY: clean test
