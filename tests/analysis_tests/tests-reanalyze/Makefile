SHELL = /bin/bash

build:
	make -C deadcode build
	make -C termination build

test:
	make -C deadcode test
	make -C termination test

clean:
	make -C deadcode clean
	make -C termination clean

.DEFAULT_GOAL := build

.PHONY: build clean clean test
