SHELL = /bin/bash

build:
	npm run build > /dev/null || true

test: build
	./test.sh

clean:
	rm -rf lib

.DEFAULT_GOAL := test

.PHONY: clean test
