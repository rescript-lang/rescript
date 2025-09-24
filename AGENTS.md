# AGENTS.md

This file provides guidance to AI coding assistants when working with code in this repository.

## Quick Start: Essential Commands

```bash
# Build and test
make && make test

# Format and check code
make format && make checkformat
```

## ⚠️ Critical Guidelines & Common Pitfalls

- **We are NOT bound by OCaml compatibility** - The ReScript compiler originated as a fork of the OCaml compiler, but we maintain our own AST and can make breaking changes. Focus on what's best for ReScript's JavaScript compilation target.

- **Never modify `parsetree0.ml`** - Existing PPX (parser extensions) rely on this frozen v0 version. When changing `parsetree.ml`, always update the mapping modules `ast_mapper_from0.ml` and `ast_mapper_to0.ml` to maintain PPX compatibility while allowing the main parsetree to evolve

- **Missing test coverage** - Always add tests for syntax, lambda, and end-to-end behavior

- **Test early and often** - Add tests immediately after modifying each compiler layer to catch problems early, rather than waiting until all changes are complete

- **Use underscore patterns carefully** - Don't use `_` patterns as lazy placeholders for new language features that then get forgotten. Only use them when you're certain the value should be ignored for that specific case. Ensure all new language features are handled correctly and completely across all compiler layers

- **Be careful with similar constructor names across different IRs** - Note that `Lam` (Lambda IR) and `Lambda` (typed lambda) have variants with similar constructor names like `Ltrywith`, but they represent different things in different compilation phases.

- **Avoid warning suppressions** - Never use `[@@warning "..."]` to silence warnings. Instead, fix the underlying issue properly

- **Do not introduce new keywords unless absolutely necessary** - Try to find ways to implement features without reserving keywords, as seen with the "catch" implementation that avoids making it a keyword.

- **No underscore if not matching on all record fields** - Do not add "; _" to pattern matches in the compiler code. OCaml warning 9 (missing fields in a record pattern) is disabled in this project.

## Compiler Architecture

### Compilation Pipeline

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

### Key Directory Structure

```
compiler/
├── syntax/          # ReScript syntax parser (MIT licensed)
├── frontend/        # AST transformations, FFI processing
├── ml/              # OCaml compiler infrastructure
├── core/            # Core compilation (lam_*, js_* files)
├── ext/             # Extended utilities and data structures
├── bsb/             # Legacy build system
└── gentype/         # TypeScript generation

analysis/            # Language server and tooling
packages/@rescript/
├── runtime/         # Runtime and standard library
└── <platform>/      # Platform-specific binaries

tests/
├── syntax_tests/    # Parser/syntax layer tests
├── tests/           # Runtime library tests
├── build_tests/     # Integration tests
└── ounit_tests/     # Compiler unit tests
```

## Working on the Compiler

### Development Workflow

1. **Understand which layer you're working on:**
   - **Syntax layer** (`compiler/syntax/`): Parsing and surface syntax
   - **ML layer** (`compiler/ml/`): Type checking and AST transformations
   - **Lambda layer** (`compiler/core/lam_*`): Intermediate representation and optimizations
   - **JS layer** (`compiler/core/js_*`): JavaScript generation

2. **Always run appropriate tests:**
   ```bash
   # For compiler or stdlib changes
   make test

   # For syntax changes
   make test-syntax

   # For specific test types
   make test-syntax-roundtrip
   make test-gentype
   make test-analysis
   ```

3. **Test your changes thoroughly:**
   - Syntax tests for new language features
   - Integration tests for behavior changes
   - Unit tests for utility functions
   - Always check JavaScript output quality

### Debugging Techniques

#### View Intermediate Representations
```bash
# Source code (for debugging preprocessing)
./cli/bsc.js -dsource myfile.res

# Parse tree (surface syntax after parsing)
./cli/bsc.js -dparsetree myfile.res

# Typed tree (after type checking)
./cli/bsc.js -dtypedtree myfile.res

# Raw lambda (unoptimized intermediate representation)
./cli/bsc.js -drawlambda myfile.res

# Use lambda printing for debugging (add in compiler/core/lam_print.ml)
```

#### Common Debug Scenarios
- **JavaScript formatting issues**: Check `compiler/ml/pprintast.ml`
- **Type checking issues**: Look in `compiler/ml/` type checker modules
- **Optimization bugs**: Check `compiler/core/lam_*.ml` analysis passes
- **Code generation bugs**: Look in `compiler/core/js_*.ml` modules

### Testing Requirements

#### When to Add Tests
- **Always** for new language features
- **Always** for bug fixes
- **When modifying** analysis passes
- **When changing** JavaScript generation

#### Test Types to Include
1. **Syntax tests** (`tests/syntax_tests/`) - Parser validation
2. **Integration tests** (`tests/tests/`) - End-to-end behavior
3. **Unit tests** (`tests/ounit_tests/`) - Compiler functions
4. **Build tests** (`tests/build_tests/`) - Error cases and edge cases
5. **Type tests** (`tests/build_tests/super_errors/`) - Type checking behavior

## Build Commands & Development

### Essential Commands
```bash
# Build compiler
make

# Build compiler in watch mode
make watch

# Build compiler and standard library
make lib

# Build compiler and standard library and run all tests
make test

# Build artifacts and update artifact list
make artifacts

# Clean build
make clean
```

### Testing Commands
```bash
# Specific test types
make test-syntax           # Syntax parser tests
make test-syntax-roundtrip # Roundtrip syntax tests
make test-gentype         # GenType tests
make test-analysis        # Analysis tests
make test-tools           # Tools tests
make test-rewatch         # Rewatch tests

# Single file debugging
./cli/bsc.js myfile.res
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


## Performance Considerations

The compiler is designed for fast feedback loops and scales to large codebases:

- **Avoid meaningless symbols** in generated JavaScript
- **Maintain readable JavaScript output**
- **Consider compilation speed impact** of changes
- **Use appropriate optimization passes** in Lambda and JS IRs
- **Profile** before and after performance-related changes

## Coding Conventions

### Naming
- **OCaml code**: snake_case (e.g., `to_string`)
- **ReScript code**: camelCase (e.g., `toString`)

### Commit Standards
- Use DCO sign-off: `Signed-Off-By: Your Name <email>`
- Include appropriate tests with all changes
- Build must pass before committing

### Code Quality
- Follow existing patterns in the codebase
- Prefer existing utility functions over reinventing
- Comment complex algorithms and non-obvious logic
- Maintain backward compatibility where possible

## Development Environment

- **OCaml**: 5.3.0+ with opam
- **Build System**: dune with profiles (dev, release, browser)
- **JavaScript**: Node.js 20+ for tooling
- **Rust**: Toolchain needed for rewatch
- **Python**: 3 required for building ninja

## Common Tasks

### Adding New Language Features
1. Update parser in `compiler/syntax/`
2. Update AST definitions in `compiler/ml/`
3. Implement type checking in `compiler/ml/`
4. Add Lambda IR handling in `compiler/core/lam_*`
5. Implement JS generation in `compiler/core/js_*`
6. Add comprehensive tests

### Debugging Compilation Issues
1. Identify which compilation phase has the issue
2. Use appropriate debugging flags (`-dparsetree`, `-dtypedtree`)
3. Check intermediate representations
4. Add debug output in relevant compiler modules
5. Verify with minimal test cases

### Working with Lambda IR
- Remember Lambda IR is the core optimization layer
- All `lam_*.ml` files process this representation
- Use `lam_print.ml` for debugging lambda expressions
- Test both with and without optimization passes