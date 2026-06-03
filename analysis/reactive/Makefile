.PHONY: build test clean

# Build the reactive library
build:
	dune build src/

# Run all tests
test:
	dune build test/reactive_test.exe
	dune exec test/reactive_test.exe

# Clean build artifacts
clean:
	dune clean
