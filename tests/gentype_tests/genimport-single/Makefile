SHELL = /bin/bash

test:
	yarn workspaces focus @tests/gentype-genimport-single
	yarn build
	# Expect direct type aliasing to imported TS type
	grep -Fq 'export type numberT = Type$$TypeScript;' src/GenTypeImportExpectedErrors.gen.tsx
	grep -Fq 'export type tupleT = Type$$TypeScript;' src/GenTypeImportExpectedErrors.gen.tsx
	grep -Fq 'export type arrayT = Type$$TypeScript;' src/GenTypeImportExpectedErrors.gen.tsx
	grep -Fq 'export type promiseT = Type$$TypeScript;' src/GenTypeImportExpectedErrors.gen.tsx
	grep -Fq 'export type nestedArrayT = Type$$TypeScript;' src/GenTypeImportExpectedErrors.gen.tsx
	grep -Fq 'export type stringT = Type$$TypeScript;' src/GenTypeImportExpectedErrors.gen.tsx
	# Now TypeScript typecheck and ensure no type errors are emitted
	yarn typecheck > ts-errors.txt 2>&1 || true
	if [ -s ts-errors.txt ]; then \
		echo 'Unexpected TypeScript errors:' ; \
		cat ts-errors.txt ; \
		exit 1 ; \
	else \
		echo 'TypeScript typecheck clean' ; \
	fi

clean:
	yarn workspaces focus @tests/gentype-genimport-single
	yarn clean

.DEFAULT_GOAL := test

.PHONY: clean test
