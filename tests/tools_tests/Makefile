SHELL = /bin/bash

build:
	yarn build

test: build
	bash ./test.sh
	yarn build

clean:
	yarn clean

.DEFAULT_GOAL := test

.PHONY: clean test
