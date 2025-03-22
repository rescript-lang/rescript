SHELL = /bin/bash

build:
	npm run build

test: build
	./test.sh

clean:
	rm -rf lib

.DEFAULT_GOAL := build

.PHONY: build clean test
