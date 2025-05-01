SHELL = /bin/bash

test:
	yarn run check
	yarn run build
	yarn run typecheck
	@git diff --quiet src/ \
		|| { echo; echo "Please review the Gentype outputs too!"; echo; false; } \
		|| exit 1

clean:
	rm -rf lib src/*.res.js src/*.gen.tsx

.DEFAULT_GOAL := test

.PHONY: clean test
