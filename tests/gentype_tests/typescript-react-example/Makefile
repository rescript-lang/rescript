SHELL = /bin/bash

test:
	yarn build
	yarn check
	yarn typecheck
	@git diff --quiet src/ \
		|| { echo; echo "Please review the Gentype outputs too!"; echo; false; } \
		|| exit 1

clean:
	yarn clean

.DEFAULT_GOAL := test

.PHONY: clean test
