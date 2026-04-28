.PHONY: build test clean

# Build the reactive library
build:
	dune build src/

# Run all tests
test:
	dune build test/ReactiveTest.exe
	dune exec test/ReactiveTest.exe

# Clean build artifacts
clean:
	dune clean

