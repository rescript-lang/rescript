# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is the ReScript compiler repository - a robustly typed language that compiles to efficient and human-readable JavaScript. ReScript is built using OCaml and includes a complete toolchain with compiler, build system, syntax parser, and standard library.

## Build Commands

### Basic Development
```bash
# Build compiler and copy executables
make

# Build in watch mode 
make watch

# Build everything including standard library
make lib

# Build artifacts and update artifact list
make artifacts
```

### Testing
```bash
# Run all tests
make test

# Run specific test types
make test-syntax           # Syntax parser tests
make test-syntax-roundtrip # Roundtrip syntax tests  
make test-gentype         # GenType tests
make test-analysis        # Analysis tests
make test-tools           # Tools tests
make test-rewatch         # Rewatch tests

# Run single file test
./cli/bsc.js myTestFile.res

# View parse/typed trees for debugging
./cli/bsc.js -dparsetree myTestFile.res
./cli/bsc.js -dtypedtree myTestFile.res
```

### Code Quality
```bash
# Format code
make format

# Check formatting
make checkformat

# Lint with Biome
npm run check
npm run check:all

# TypeScript type checking  
npm run typecheck
```

### Clean Operations
```bash
make clean      # Clean OCaml build artifacts
make clean-all  # Clean everything including Rust/gentype
```

## Compiler Architecture

The ReScript compiler follows this high-level pipeline:

```
ReScript Source (.res)
  ↓ (ReScript Parser - compiler/syntax/)
Surface Syntax Tree  
  ↓ (Frontend transformations - compiler/frontend/)
Surface Syntax Tree
  ↓ (OCaml Type Checker - compiler/ml/)
Typedtree
  ↓ (Lambda compilation - compiler/core/lam_*)
Lambda IR
  ↓ (JS compilation - compiler/core/js_*)  
JS IR
  ↓ (JS output - compiler/core/js_dump*)
JavaScript Code
```

### Key Directories

- **`compiler/syntax/`** - ReScript syntax parser (MIT licensed, separate from main LGPL)
- **`compiler/frontend/`** - AST transformations, external FFI processing, built-in attributes
- **`compiler/ml/`** - OCaml compiler infrastructure (type checker, typedtree, etc.)
- **`compiler/core/`** - Core compilation:
  - `lam_*` files: Lambda IR compilation and optimization passes
  - `js_*` files: JavaScript IR and code generation
- **`compiler/ext/`** - Extended utilities and data structures
- **`compiler/bsb/`** - Build system implementation
- **`compiler/gentype/`** - TypeScript generation
- **`runtime/`** - ReScript standard library (written in ReScript)
- **`lib/`** - Compiled JavaScript output of standard library
- **`analysis/`** - Language server and tooling support

### Build System Components

- **`compiler/bsb_exe/`** - Main ReScript build tool entry point
- **`compiler/bsc/`** - Compiler binary entry point  
- **`rewatch/`** - File watcher (written in Rust)
- **`ninja/`** - Vendored Ninja build system

## Development Setup Notes

- Uses OCaml 5.3.0+ with opam for compiler development
- Uses dune as build system with specific profiles (dev, release, browser)
- Node.js 20+ required for JavaScript tooling
- Rust toolchain needed for rewatch file watcher
- Python ≤3.11 required for building ninja

## Coding Conventions

- **OCaml code**: snake_case (e.g., `to_string`)
- **ReScript code**: camelCase (e.g., `toString`)
- Use DCO sign-off for all commits: `Signed-Off-By: Your Name <email>`

## Testing Strategy

- **Mocha tests** (`tests/tests/`) - Runtime library unit tests
- **Build system tests** (`tests/build_tests/`) - Integration tests  
- **OUnit tests** (`tests/ounit_tests/`) - Compiler unit tests
- **Expectation tests** - Plain `.res` files that check compilation output
- Always include appropriate tests with new features/changes

## Performance Notes

The compiler is designed for fast feedback loops and scales to large codebases. When making changes:
- Avoid introducing meaningless symbols
- Maintain readable JavaScript output
- Consider compilation speed impact
- Use appropriate optimization passes in the Lambda and JS IRs