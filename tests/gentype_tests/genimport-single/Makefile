SHELL = /bin/bash

test:
	yarn workspaces focus @tests/gentype-genimport-single
	yarn build
	# Helper exists once in single generated file
	grep -Fq 'type $$GenTypeImport<Expected, T extends Expected> = T;' src/GenTypeImportExpectedErrors.gen.tsx
	# Wrapper lines for each case present
	grep -Fq 'export type numberT = $$GenTypeImport<number,' src/GenTypeImportExpectedErrors.gen.tsx
	grep -Fq 'export type tupleT = $$GenTypeImport<[number, string],' src/GenTypeImportExpectedErrors.gen.tsx
	grep -Fq 'export type arrayT = $$GenTypeImport<number[],' src/GenTypeImportExpectedErrors.gen.tsx
	grep -Fq 'export type promiseT = $$GenTypeImport<Promise<number>,' src/GenTypeImportExpectedErrors.gen.tsx
	# nested arrays may render as Array<number[]>; accept either style
	grep -Fq 'export type nestedArrayT = $$GenTypeImport<Array<number[]>' src/GenTypeImportExpectedErrors.gen.tsx
	# Positive wrapper present too
	grep -Fq 'export type stringT = $$GenTypeImport<string,' src/GenTypeImportExpectedErrors.gen.tsx
	# Now TypeScript typecheck and assert exact errors
	yarn typecheck > ts-errors.txt 2>&1 || true
	# Expect exactly 5 TS2344 errors (the mismatches), and no others
	test $$(grep -c 'error TS2344' ts-errors.txt) -eq 5
	grep -Fq "Type 'string' does not satisfy the constraint 'number'." ts-errors.txt
	grep -Fq "Type 'string' does not satisfy the constraint '[number, string]'." ts-errors.txt
	grep -Fq "Type 'string' does not satisfy the constraint 'number[]'." ts-errors.txt
	grep -Fq "Type 'string' does not satisfy the constraint 'Promise<number>'." ts-errors.txt
	grep -Fq "Type 'string' does not satisfy the constraint 'number[][]'." ts-errors.txt
	# Positive should not produce any error about constraint 'string'
	if grep -Fq "constraint 'string'." ts-errors.txt; then \
		echo 'Unexpected error for positive string case' ; \
		exit 1 ; \
	else \
		echo 'All expected TS errors present; positive has no error' ; \
	fi

clean:
	yarn workspaces focus @tests/gentype-genimport-single
	yarn clean

.DEFAULT_GOAL := test

.PHONY: clean test
