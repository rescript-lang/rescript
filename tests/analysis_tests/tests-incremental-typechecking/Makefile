SHELL = /bin/bash

node_modules/.bin/rescript:
	npm install

build: node_modules/.bin/rescript
	node_modules/.bin/rescript > /dev/null || true

test: build
	./test.sh

clean:
	rm -rf node_modules lib

.DEFAULT_GOAL := test

.PHONY: clean test
