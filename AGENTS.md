# AGENTS.md

This file provides guidance to AI coding assistants when working with code in this repository.

## Quick Start: Essential Commands

```bash
# Build the platform toolchain (default target)
make

# Build the platform toolchain + stdlib
make lib

# Build the platform toolchain + stdlib and run tests
make test

# Format code
make format

# Check formatting
make checkformat
```

The Makefile’s targets build on each other in this order:

1. `yarn-install` runs automatically for targets that need JavaScript tooling (lib, playground, tests, formatting, etc.).
2. `build` (default target) builds the toolchain binaries (all copied into `packages/@rescript/<platform>/bin`):
   - `compiler` builds the dune executables (`bsc`, `rescript-*`, `ounit_tests`, etc.).
   - `rewatch` builds the Rust-based ReScript build system and CLI.
3. `lib` uses those toolchain outputs to build the runtime sources.
4. Test targets (`make test`, `make test-syntax`, etc.) reuse everything above.

## ⚠️ Critical Guidelines & Common Pitfalls

- **We are NOT bound by OCaml compatibility** - The ReScript compiler originated as a fork of the OCaml compiler, but we maintain our own AST and can make breaking changes. Focus on what's best for ReScript's JavaScript compilation target.

- **Never modify `parsetree0.ml`** - Existing PPX (parser extensions) rely on this frozen v0 version. When changing `parsetree.ml`, always update the mapping modules `ast_mapper_from0.ml` and `ast_mapper_to0.ml` to maintain PPX compatibility while allowing the main parsetree to evolve

- **Missing test coverage** - Always add tests for syntax, lambda, and end-to-end behavior

- **Test early and often** - Add tests immediately after modifying each compiler layer to catch problems early, rather than waiting until all changes are complete

- **Use underscore patterns carefully** - Don't use `_` patterns as lazy placeholders for new language features that then get forgotten. Only use them when you're certain the value should be ignored for that specific case. Ensure all new language features are handled correctly and completely across all compiler layers
- **Avoid `let _ = …` for side effects** - If you need to call a function only for its side effects, use `ignore expr` (or bind the result and thread state explicitly). Do not write `let _ = expr in ()`, and do not discard stateful results—plumb them through instead.

- **Don't use unit `()` with mandatory labeled arguments** - When a function has a mandatory labeled argument (like `~config`), don't add a trailing `()` parameter. The labeled argument already prevents accidental partial application. Only use `()` when all parameters are optional and you need to force evaluation. Example: `let forceDelayedItems ~config = ...` not `let forceDelayedItems ~config () = ...`

- **Be careful with similar constructor names across different IRs** - Note that `Lam` (Lambda IR) and `Lambda` (typed lambda) have variants with similar constructor names like `Ltrywith`, but they represent different things in different compilation phases.

- **Avoid warning suppressions** - Never use `[@@warning "..."]` to silence warnings. Instead, fix the underlying issue properly
- **Skip trailing `; _` in record patterns** - The warning it targets is disabled in this codebase, so prefer `{field = x}` over `{field = x; _}`.

- **Do not introduce new keywords unless absolutely necessary** - Try to find ways to implement features without reserving keywords, as seen with the "catch" implementation that avoids making it a keyword.

- **Use tight timeouts when testing compiler/parser** - When running the compiler or parser in tests, always use a short timeout (e.g., 5-10 seconds). Compilation should be fast, and logic errors can cause infinite loops or hangs. A tight timeout ensures these issues are caught quickly rather than blocking indefinitely.

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

## Working on the Build System

### Rewatch Architecture

Rewatch is ReScript's build system written in Rust. It provides fast incremental builds, better error messages, and improved developer experience.

#### Key Components

```
rewatch/src/
├── build/              # Core build system logic
│   ├── build_types.rs  # Core data structures (BuildState, Module, etc.)
│   ├── compile.rs      # Compilation logic and bsc argument generation
│   ├── parse.rs        # AST generation and parser argument handling
│   ├── packages.rs     # Package discovery and dependency resolution
│   ├── deps.rs         # Dependency analysis and module graph
│   ├── clean.rs        # Build artifact cleanup
│   └── logs.rs         # Build logging and error reporting
├── cli.rs              # Command-line interface definitions
├── config.rs           # rescript.json configuration parsing
├── watcher.rs          # File watching and incremental builds
└── main.rs             # Application entry point
```

#### Build System Flow

1. **Initialization** (`build::initialize_build`)
   - Parse `rescript.json` configuration
   - Discover packages and dependencies
   - Set up compiler information
   - Create initial `BuildState`

2. **AST Generation** (`build::parse`)
   - Generate AST files using `bsc -bs-ast`
   - Handle PPX transformations
   - Process JSX

3. **Dependency Analysis** (`build::deps`)
   - Analyze module dependencies from AST files
   - Build dependency graph
   - Detect circular dependencies

4. **Compilation** (`build::compile`)
   - Generate `bsc` compiler arguments
   - Compile modules in dependency order
   - Handle warnings and errors
   - Generate JavaScript output

5. **Incremental Updates** (`watcher.rs`)
   - Watch for file changes
   - Determine dirty modules
   - Recompile only affected modules

### Development Guidelines

#### Adding New Features

1. **CLI Arguments**: Add to `cli.rs` in `BuildArgs` and `WatchArgs`
2. **Configuration**: Extend `config.rs` for new `rescript.json` fields
3. **Build Logic**: Modify appropriate `build/*.rs` modules
4. **Thread Parameters**: Pass new parameters through the build system chain
5. **Add Tests**: Include unit tests for new functionality

#### Common Patterns

- **Parameter Threading**: New CLI flags need to be passed through:
  - `main.rs` → `build::build()` → `initialize_build()` → `BuildState`
  - `main.rs` → `watcher::start()` → `async_watch()` → `initialize_build()`

- **Configuration Precedence**: Command-line flags override `rescript.json` config
- **Error Handling**: Use `anyhow::Result` for error propagation
- **Logging**: Use `log::debug!` for development debugging

#### Testing

```bash
# Run rewatch tests (from project root)
cargo test --manifest-path rewatch/Cargo.toml

# Test specific functionality
cargo test --manifest-path rewatch/Cargo.toml config::tests::test_get_warning_args

# Run clippy for code quality
cargo clippy --manifest-path rewatch/Cargo.toml --all-targets --all-features

# Check formatting
cargo fmt --check --manifest-path rewatch/Cargo.toml

# Build rewatch
cargo build --manifest-path rewatch/Cargo.toml --release

# Or use the Makefile shortcuts
make rewatch          # Build rewatch
make test-rewatch     # Run integration tests
```

**Note**: The rewatch project is located in the `rewatch/` directory with its own `Cargo.toml` file. All cargo commands should be run from the project root using the `--manifest-path rewatch/Cargo.toml` flag, as shown in the CI workflow.

**Integration Tests**: The `make test-rewatch` command runs bash-based integration tests located in `rewatch/tests/suite.sh`. These tests use the `rewatch/testrepo/` directory as a test workspace with various package configurations to verify rewatch's behavior across different scenarios.

#### Debugging

- **Build State**: Use `log::debug!` to inspect `BuildState` contents
- **Compiler Args**: Check generated `bsc` arguments in `compile.rs`
- **Dependencies**: Inspect module dependency graph in `deps.rs`
- **File Watching**: Monitor file change events in `watcher.rs`

#### Performance Considerations

- **Incremental Builds**: Only recompile dirty modules
- **Parallel Compilation**: Use `rayon` for parallel processing
- **Memory Usage**: Be mindful of `BuildState` size in large projects
- **File I/O**: Minimize file system operations

#### Performance vs Code Quality Trade-offs

When clippy suggests refactoring that could impact performance, consider the trade-offs:

- **Parameter Structs vs Many Arguments**: While clippy prefers parameter structs for functions with many arguments, sometimes the added complexity isn't worth it. Use `#[allow(clippy::too_many_arguments)]` for functions that legitimately need many parameters and where a struct would add unnecessary complexity.

- **Cloning vs Borrowing**: Sometimes cloning is necessary due to Rust's borrow checker rules. If the clone is:
  - Small and one-time (e.g., `Vec<String>` with few elements)
  - Necessary for correct ownership semantics
  - Not in a hot path
  
  Then accept the clone rather than over-engineering the solution.

- **When to Optimize**: Profile before optimizing. Most "performance concerns" in build systems are negligible compared to actual compilation time.

- **Avoid Unnecessary Type Conversions**: When threading parameters through multiple function calls, use consistent types (e.g., `String` throughout) rather than converting between `String` and `&str` at each boundary. This eliminates unnecessary allocations and conversions.

### Common Tasks

#### Adding New CLI Flags

1. Add to `BuildArgs` and `WatchArgs` in `cli.rs`
2. Update `From<BuildArgs> for WatchArgs` implementation
3. Pass through `main.rs` to build functions
4. Thread through build system to where it's needed
5. Add unit tests for the new functionality

#### Modifying Compiler Arguments

1. Update `compiler_args()` in `build/compile.rs`
2. Consider both parsing and compilation phases
3. Handle precedence between CLI flags and config
4. Test with various `rescript.json` configurations

#### Working with Dependencies

1. Use `packages.rs` for package discovery
2. Update `deps.rs` for dependency analysis
3. Handle both local and external dependencies
4. Consider dev dependencies vs regular dependencies

#### File Watching

1. Modify `watcher.rs` for file change handling
2. Update `AsyncWatchArgs` for new parameters
3. Handle different file types (`.res`, `.resi`, etc.)
4. Consider performance impact of watching many files

---

## OCaml to Rust Compiler Rewrite

The compiler is being rewritten from OCaml to Rust. This is a major undertaking documented in the `compiler-rust/` directory.

### Key Documents

- **[compiler-rust/PLAN.md](compiler-rust/PLAN.md)** - Comprehensive migration plan with phases, technical approach, and success criteria
- **[compiler-rust/PROGRESS.md](compiler-rust/PROGRESS.md)** - Current progress, completed tasks, and next steps
- **[compiler-rust/GLOBAL_STATE_AUDIT.md](compiler-rust/GLOBAL_STATE_AUDIT.md)** - Catalog of OCaml global state to eliminate

> **Note for AI assistants**: These documents are living documents. Update `PLAN.md` when you discover new insights about architecture, challenges, or better approaches. Update `PROGRESS.md` as you complete tasks or identify new work items. Add to `GLOBAL_STATE_AUDIT.md` when you find additional global state that needs to be eliminated.

### Rewrite Constraints

1. **No global state** - The Rust compiler must support concurrent compilation
2. **Drop-in replacement** - Must be backward compatible at all times
3. **Identical output** - JavaScript output must be byte-for-byte identical

### Migration Phases

1. **Phase 1: Foundation** - Core types, contexts, FFI boundary (`compiler/ext/` → Rust)
2. **Phase 2: Parser** - ReScript parser in Rust (`compiler/syntax/` → Rust)
3. **Phase 3: Backend** - Lambda IR and JS generation (`compiler/core/` → Rust)
4. **Phase 4: Type Checker** - Type system in Rust (`compiler/ml/` → Rust)
5. **Phase 5: Integration** - Single Rust binary, OCaml removed

### Development Commands

```bash
# Build the Rust compiler crate
cargo build --manifest-path compiler-rust/Cargo.toml

# Run tests
cargo test --manifest-path compiler-rust/Cargo.toml

# Run tests with thread sanitizer (nightly)
RUSTFLAGS="-Z sanitizer=thread" cargo test --manifest-path compiler-rust/Cargo.toml

# Check for issues
cargo clippy --manifest-path compiler-rust/Cargo.toml --all-targets
```

### Debugging the Rust Compiler

```bash
# View Lambda IR from Rust compiler (prints to stderr)
./compiler-rust/target/debug/bsc -drawlambda myfile.res

# View Lambda IR from OCaml reference compiler (for comparison)
packages/@rescript/darwin-arm64/bin/bsc.exe -drawlambda myfile.res

# Compile with Rust compiler and check JS output
./compiler-rust/target/debug/bsc myfile.res && cat myfile.js

# Compile with OCaml reference compiler for comparison
packages/@rescript/darwin-arm64/bin/bsc.exe myfile.res
```

### Following the Reference Implementation

**IMPORTANT**: When implementing features in the Rust compiler, always compare against the OCaml reference implementation:

1. **Lambda IR comparison** - Use `-drawlambda` on both compilers to compare the intermediate representation
2. **JS output comparison** - The Rust compiler should produce functionally equivalent (ideally identical) JavaScript output
3. **Study the OCaml code** - When in doubt, read the corresponding OCaml implementation in `compiler/core/` (for Lambda/JS) or `compiler/ml/` (for type checking)
4. **Test with the same inputs** - Run both compilers on the same `.res` files to verify behavior matches

The goal is byte-for-byte identical output where possible, or at minimum functionally equivalent output.

### Syntax Tests for Rust Parser

The syntax tests compare parser/printer output between the Rust and OCaml implementations.

**⚠️ CRITICAL: NEVER modify or commit changes to expected files**

The expected files in `tests/syntax_tests/data/*/expected/` contain the OCaml reference output. These files define the target behavior that the Rust implementation must match.

- **NEVER** update expected files to match Rust output - this defeats the purpose of parity testing
- **NEVER** commit changes to expected files on feature branches
- **ALWAYS** revert expected files to master before running tests: `git checkout master -- tests/syntax_tests/data/`
- If tests fail, **fix the Rust implementation**, not the expected files

The test script (`scripts/test_syntax.sh`) overwrites expected files when run. Always revert them afterward.

#### Test Categories and Flags

The test script (`scripts/test_syntax.sh`) uses different flags for different test categories:

| Directory | Flags | Output Format |
|-----------|-------|---------------|
| `parsing/errors`, `parsing/infiniteLoops`, `parsing/recovery` | `-recover -print ml` | ML format (OCaml AST) |
| `parsing/grammar`, `parsing/other` | `-print ml` | ML format (OCaml AST) |
| `printer/*`, `conversion/*` | (default) | ReScript format |
| `ast-mapping/*` | `-test-ast-conversion -jsx-version 4` | ReScript format with AST roundtrip |
| `ppx/react/*` | `-jsx-version 4` | ReScript format with JSX transform |

#### Running Tests Against Master Reference

```bash
# Build Rust parser
cargo build --manifest-path compiler-rust/Cargo.toml --release

# Test a single file against master expected output
f="tests/syntax_tests/data/printer/expr/apply.res"
dir=$(dirname "$f")
name=$(basename "$f")
expected_path="${dir}/expected/${name}.txt"

# Compare Rust output vs original master expected
diff <(./compiler-rust/target/release/res_parser_rust "$f") \
     <(git show "master:$expected_path")

# For parsing tests (use -print ml)
diff <(./compiler-rust/target/release/res_parser_rust -p ml "$f") \
     <(git show "master:$expected_path")
```

#### Batch Testing Against Master

```bash
# Test printer/* against master expectations
BINARY="./compiler-rust/target/release/res_parser_rust"
PASSED=0; FAILED=0
while IFS= read -r f; do
  dir=$(dirname "$f"); name=$(basename "$f")
  expected_path="${dir}/expected/${name}.txt"
  original=$(git show "master:$expected_path" 2>/dev/null) || continue
  actual=$("$BINARY" "$f" 2>&1)
  [ "$actual" = "$original" ] && ((PASSED++)) || ((FAILED++))
done < <(find tests/syntax_tests/data/printer -name "*.res")
echo "Passed: $PASSED / $((PASSED + FAILED))"
```

#### Handling Expected Files

1. **Never blindly update expected files** - If tests fail, fix the Rust implementation to match OCaml output
2. **Use master as reference** - `git show master:path/to/expected/file.txt` gives the correct expected output
3. **Check if files are modified** - `git diff master -- tests/syntax_tests/data/*/expected/` shows what's been changed
4. **Reset to master if needed** - `git checkout master -- tests/syntax_tests/data/*/expected/` restores original expectations

#### Common Printer Differences to Fix

When the Rust printer output differs from OCaml, common issues include:
- **Exotic identifiers**: `~~~compare` should not become `\"~~~"(compare)`
- **Multi-line formatting**: Record/array breaking differs
- **Parenthesization**: Extra parens around expressions
- **Comment preservation**: Comments in source should appear in ReScript output
- **Blank line preservation**: Spacing between items should match

### Key Design Decisions

- **Thread-safe ID generation** - `IdGenerator` with `AtomicI32` instead of global `currentstamp`
- **Explicit contexts** - `CompilationContext`, `TypeContext` passed through pipeline
- **Arena-per-compilation** - Type arenas owned by `TypeContext`, not shared
- **No `static mut`** - All mutable state in explicit contexts
