SHELL = /bin/bash

node_modules/.bin/rescript:
	npm install

build: node_modules/.bin/rescript
	node_modules/.bin/rescript

test: build node_modules/.bin/rescript
	./test.sh

clean:
	rm -rf node_modules lib

.DEFAULT_GOAL := build

.PHONY: build clean test
