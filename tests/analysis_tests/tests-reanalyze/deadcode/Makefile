SHELL = /bin/bash

build:
	yarn build

test: build node_modules/.bin/rescript
	./test.sh

clean:
	yarn clean

.DEFAULT_GOAL := build

.PHONY: build clean test
