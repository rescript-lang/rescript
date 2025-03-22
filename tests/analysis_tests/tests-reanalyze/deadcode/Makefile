SHELL = /bin/bash

build:
	npm run build

test: build node_modules/.bin/rescript
	./test.sh

clean:
	rm -rf lib

.DEFAULT_GOAL := build

.PHONY: build clean test
