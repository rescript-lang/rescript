# AGENTS.md

This file provides guidance to AI coding assistants when working with code in this repository.

## ⚠️ Repository Setup - READ FIRST

This repository has two Git remotes configured:

- `origin` → `rescript-lang/rescript` (upstream public repo - **READ ONLY**)
- `private` → `jfrolich/rescript-rust` (private development repo)

**IMPORTANT: Only push to the `private` remote.** Never push directly to `origin`.

```bash
# Push changes to private repo
git push private

# Pull updates from upstream (if needed)
git fetch origin
git merge origin/master
```

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

- **COMMIT YOUR CHANGES** - After completing ANY meaningful work (bug fix, feature, refactor, parity improvement), you MUST commit immediately. Do not leave work uncommitted. Do not move on to the next task without committing first. This is the most important rule.

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

- **Handle concurrent agent work gracefully** - If compilation fails due to errors in code you didn't touch, another agent may be actively working on that part of the codebase. Do NOT modify their files and NEVER use `git stash`. Simply wait 30 seconds and retry the build. Repeat this until compilation succeeds—the other agent will fix their changes shortly.

- **Do NOT revert large changes** - If a task impacts a large amount of code, do NOT revert and give up. Complete the large task as requested. The user is aware it's a big change and expects you to follow through.

- **Do NOT switch tasks when things get hard** - If you're working on a difficult task, do NOT abandon it to work on something easier. Complete the current task FIRST, then move on to the next one. Switching tasks loses valuable context and leaves work incomplete.

- **ALWAYS commit before moving on** - Before starting a new task, before investigating a problem, before taking a break: COMMIT. Use `git add <files> && git commit -m "description"`. No exceptions.

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
- **Commit atomically** - Each commit should address exactly one concern (one bug fix, one feature, one refactor). When making multiple related changes, split them into separate atomic commits. For example, if fixing a parity issue requires adding a new field to a struct and then using that field in parsing logic, make two commits: one for the struct change, one for the parsing logic. This makes it easier to review changes, bisect issues, revert specific changes if needed, and understand the evolution of the codebase. Each commit message should clearly describe what was changed and why.

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

**CRITICAL PRINCIPLE**: The Rust parser is a **1:1 rewrite** of the OCaml parser. All logic must be the same, translated into idiomatic Rust. The **only** architectural difference is eliminating global state to enable parallel compilation. Do not "improve" algorithms, change data structures, or deviate from the OCaml implementation's approach—even if you think there's a better way. Parity is the priority; optimizations can come later after we have identical behavior.

**IMPORTANT**: When implementing features in the Rust compiler, always compare against the OCaml reference implementation:

1. **Lambda IR comparison** - Use `-drawlambda` on both compilers to compare the intermediate representation
2. **JS output comparison** - The Rust compiler should produce functionally equivalent (ideally identical) JavaScript output
3. **Study the OCaml code** - When in doubt, read the corresponding OCaml implementation in `compiler/core/` (for Lambda/JS) or `compiler/ml/` (for type checking)
4. **Test with the same inputs** - Run both compilers on the same `.res` files to verify behavior matches

The goal is byte-for-byte identical output where possible, or at minimum functionally equivalent output.

- **Investigate parity differences at the source** - When you find a difference between Rust and OCaml output, don't just try to match the output superficially. Read the corresponding OCaml implementation to understand *why* it produces that output. The root cause is often in a different place than where the symptom appears.

- **Study the OCaml code, not just the output** - When fixing parity issues, you must read and understand the actual OCaml implementation, not just try to reverse-engineer what it does from the output. The goal is to create an *equivalent* Rust implementation that follows the same logic and handles the same edge cases. Simply tweaking Rust code until the output matches is fragile and will miss subtle behaviors. Understand the algorithm, data structures, and invariants in the OCaml code first, then write idiomatic Rust that achieves the same result.

- **Stay focused on hard parity issues** - If achieving parity requires a significant change (refactoring, adding new infrastructure, rethinking an approach), do that work rather than abandoning it for simpler issues. The end goal is 100% parity, and all issues must eventually be fixed. Switching between tasks loses valuable context about the problem you were investigating. It's better to complete a difficult task—even if it takes substantial effort—than to context-switch repeatedly and never finish the hard parts.

- **Commit parity improvements atomically** - When parity increases (more tests pass), commit immediately with atomic commits. Each commit should address one specific fix (e.g., "fix location for recursive module declarations" not "fix various location issues"). This creates a clear history of progress, makes it easy to bisect regressions, and ensures no work is lost. If a fix requires multiple steps (e.g., adding a struct field, then using it), make separate commits for each step.

### Syntax Tests for Rust Parser

The syntax tests compare parser/printer output between the Rust and OCaml implementations.

**⚠️ CRITICAL: NEVER modify or commit changes to expected files**

The expected files in `tests/syntax_tests/data/*/expected/` contain the OCaml reference output. These files define the target behavior that the Rust implementation must match.

- **NEVER** update expected files to match Rust output - this defeats the purpose of parity testing
- **NEVER** commit changes to expected files on feature branches
- If tests fail, **fix the Rust implementation**, not the expected files

#### Running the Syntax Tests

```bash
# Build the Rust parser first
cargo build --manifest-path compiler-rust/Cargo.toml --release

# Run all syntax tests with the Rust parser
PARSER=rust ./scripts/test_syntax.sh
```

The test script will:
1. Run the Rust parser on all test files
2. Compare output against expected files (without modifying them)
3. Show a summary table of pass/fail by category
4. Preserve test artifacts in `tests/temp/` for inspection

Example output:
```
=== Rust Parser Syntax Test Results ===

Category                  Passed Failed  Total
--------                  ------ ------  -----
parsing/errors                 2     82     84  ( 2%)
parsing/grammar               14    121    135  (10%)
printer                       16    171    187  ( 8%)
...
--------                  ------ ------  -----
TOTAL                         47    459    506  ( 9%)
```

#### Picking Up a Failing Test to Fix

After running the tests, use these commands to find and inspect failures:

```bash
# List all failing tests
grep '^FAIL:' tests/temp/results.txt | cut -d: -f2

# View diff for a specific failing test
diff tests/syntax_tests/data/printer/expr/apply.res.txt \
     tests/temp/syntax_tests/data/printer/expr/apply.res.txt

# View the source file being tested
cat tests/syntax_tests/data/printer/expr/apply.res

# View the expected output (OCaml reference)
cat tests/syntax_tests/data/printer/expr/expected/apply.res.txt

# View the actual Rust output
cat tests/temp/syntax_tests/data/printer/expr/expected/apply.res.txt
```

#### Test a Single File Manually

```bash
# For printer tests (default ReScript output)
./compiler-rust/target/release/res_parser_rust tests/syntax_tests/data/printer/expr/apply.res

# For parsing tests (ML format output)
./compiler-rust/target/release/res_parser_rust -print ml tests/syntax_tests/data/parsing/grammar/expressions/binary.res

# Compare with OCaml parser output
./_build/install/default/bin/res_parser tests/syntax_tests/data/printer/expr/apply.res
```

#### Test Categories and Flags

The test script uses different flags for different test categories:

| Directory | Flags | Output Format |
|-----------|-------|---------------|
| `parsing/errors`, `parsing/infiniteLoops`, `parsing/recovery` | `-recover -print ml` | ML format (OCaml AST) |
| `parsing/grammar`, `parsing/other` | `-print ml` | ML format (OCaml AST) |
| `printer/*`, `conversion/*` | (default) | ReScript format |
| `ast-mapping/*` | `-test-ast-conversion -jsx-version 4` | ReScript format with AST roundtrip |
| `ppx/react/*` | `-jsx-version 4` | ReScript format with JSX transform |

#### Common Printer Differences to Fix

When the Rust printer output differs from OCaml, common issues include:
- **Comment placement**: Comments should stay attached to the correct AST nodes
- **Whitespace/newlines**: Blank lines between items should be preserved
- **Multi-line formatting**: Record/array breaking should match
- **Parenthesization**: Extra or missing parens around expressions
- **Exotic identifiers**: `~~~compare` should not become `\"~~~"(compare)`

### AST Parity Test Suite

The AST parity test suite (`scripts/test_parser_ast_parity.sh`) comprehensively compares the Rust and OCaml parser outputs across multiple dimensions:

**Tests performed:**
1. **sexp parity** - AST structure matches between OCaml and Rust
2. **sexp-locs parity** - AST structure + locations match
3. **sexp0-locs parity** - parsetree→parsetree0→sexp output matches (single-trip)
4. **Roundtrip structure parity** - After parsetree→parsetree0→parsetree, both parsers produce same AST
5. **Roundtrip location parity** - Locations preserved correctly through parsetree0 conversion
6. **Binary parity** - Marshalled parsetree0 is byte-identical (critical for PPX compatibility)

#### Running the AST Parity Tests

```bash
# Run all tests (default: 16 parallel jobs)
./scripts/test_parser_ast_parity.sh

# Test a single file (fastest for iteration)
./scripts/test_parser_ast_parity.sh tests/syntax_tests/data/printer/expr/apply.res

# Test files matching a pattern
./scripts/test_parser_ast_parity.sh "printer/expr"

# Quick mode - stop on first failure
./scripts/test_parser_ast_parity.sh --quick

# Verbose mode - show each file being tested
./scripts/test_parser_ast_parity.sh -v tests/syntax_tests/data/printer/expr/apply.res

# Adjust parallelism
./scripts/test_parser_ast_parity.sh -j8
```

#### Regression Tracking

The parity test suite tracks regressions using baseline files in `tests/parity_baselines/`. Each test type has its own baseline:

- `sexp_locs.txt` - Files passing sexp-locs parity
- `binary.txt` - Files passing binary parity
- `sexp0_locs.txt` - Files passing sexp0-locs parity
- `binary0.txt` - Files passing binary0 parity
- `roundtrip_structure.txt` - Files passing roundtrip structure parity
- `roundtrip_locs.txt` - Files passing roundtrip location parity

**How it works:**
1. When a file passes a test, it's added to the corresponding baseline (append-only)
2. On subsequent runs, if a file that's in the baseline fails, it's flagged as a **regression**
3. Regressions are reported prominently and cause the test to fail

**Why this matters:**
- Prevents accidentally breaking files that were already working
- Makes it easy to see which files regressed after a change
- Baselines grow monotonically as parity improves
- Committed to git so regressions are caught in CI/PRs

**When you see a regression:**
1. The output will clearly list which files regressed for each test type
2. Fix the regression before moving on - don't let parity decrease
3. The baseline files should never be manually edited to remove entries

#### Quick Iteration Workflow

When fixing parity issues, use single-file mode for fast feedback:

```bash
# 1. Find a failing file from the test output
# 2. Test just that file
./scripts/test_parser_ast_parity.sh tests/syntax_tests/data/printer/expr/apply.res

# 3. Make changes to Rust parser
# 4. Rebuild
cargo build --manifest-path compiler-rust/Cargo.toml --release

# 5. Re-test the single file
./scripts/test_parser_ast_parity.sh tests/syntax_tests/data/printer/expr/apply.res
```

#### Known sexp-locs Parity Issues (5 files remaining at 99.5%)

The following files have known location parity differences that require more complex fixes:

1. **DocComments.res** - Doc comment attribute location handling
   - Issue: Attribute end positions differ; type locations with doc comments have wrong start positions
   - Root cause: Doc comments need different location tracking in attribute parsing

2. **UncurriedByDefault.res** - Ghost locations vs real locations
   - Issue: OCaml uses ghost location `(loc 1 -1 1 -1)` for synthesized unit types, Rust uses real locations
   - Root cause: Need to identify when OCaml synthesizes types and use `Location::none()`

3. **underscoreApply.res** - Underscore-apply transformation locations
   - Issue: Synthetic `__x` variable locations differ (OCaml uses last `_`, Rust uses first)
   - Root cause: Underscore-apply PPX transformation needs location adjustment

4. **attributes.res** - Attribute location end positions
   - Issue: Structure item location end positions and attribute locations differ by 1-3 characters
   - Root cause: Attribute payload parsing location handling

5. **variant.res** - Missing doc comment attributes
   - Issue: Doc comments on variant constructors not being converted to `res.doc` attributes
   - Root cause: Doc comment to attribute conversion for variant constructor contexts

### Key Design Decisions

- **Thread-safe ID generation** - `IdGenerator` with `AtomicI32` instead of global `currentstamp`
- **Explicit contexts** - `CompilationContext`, `TypeContext` passed through pipeline
- **Arena-per-compilation** - Type arenas owned by `TypeContext`, not shared
- **No `static mut`** - All mutable state in explicit contexts

### Binary AST Parity (Completed for Current Parsetree)

**Status:** ✅ Current parsetree format (the active AST format) has 100% binary parity.

The binary AST format uses OCaml's Marshal serialization, which includes back-references for shared objects. When OCaml creates two references to the same memory object, Marshal writes a shared reference (CODE_SHARED8/16/32) instead of duplicating the data.

**What's working:**
- Current parsetree binary: 100% parity (1049/1049 files identical)
- sexp-locs: 100% parity (1049/1049 files identical)

**parsetree0 / binary0 - NOT A PRIORITY:**

⚠️ **Do not work on parsetree0 or binary0 parity at this time.** These involve the frozen PPX-compatible format and will be addressed later. The code must still compile, but parity differences are expected and acceptable for now.

- binary0 has 985 differing files (this is fine)
- roundtrip (parsetree→parsetree0→parsetree) has differences (this is fine)

**Key implementation points:**

- **ParseArena** (`compiler-rust/src/parse_arena.rs`): Arena for positions and locations. Same `PosIdx`/`LocIdx` = same object for marshalling purposes.
  - The "none" position/location are pre-allocated at index 0

- **Marshal writer** (`compiler-rust/src/binary_ast/marshal.rs`): Tables for sharing:
  - `position_idx_table`: Maps `PosIdx` to object counter for arena-based position sharing
  - `location_idx_table`: Maps `LocIdx` to object counter for arena-based location sharing

**Testing:**

```bash
# Run full parity test suite
./scripts/test_parser_ast_parity.sh

# Test a single file for quick iteration
./scripts/test_parser_ast_parity.sh tests/syntax_tests/data/printer/expr/apply.res

# Compare binary output directly
./_build/install/default/bin/res_parser -print binary FILE > /tmp/ocaml.bin
./compiler-rust/target/release/res_parser_rust -print binary FILE > /tmp/rust.bin
diff <(xxd /tmp/ocaml.bin) <(xxd /tmp/rust.bin)
```

**Debugging tips:**

- When binary differs, look at the hex dump: shared references show up as `04 XX` (CODE_SHARED8), `05 XX XX` (CODE_SHARED16)
- The offset after the opcode is the "distance back" in object counter terms
- If Rust has more bytes, it's likely not sharing something OCaml shares
- If Rust has fewer bytes, it's likely sharing something OCaml doesn't share
- Add debug prints in `marshal.rs` to trace object counter values

**Understanding OCaml's value sharing (critical for parity):**

OCaml's Marshal format uses **pointer identity** for sharing. When two AST nodes reference the same memory object (same pointer), Marshal writes a shared reference instead of duplicating the data. This is NOT about string interning or content equality—it's about whether the OCaml code constructs a NEW value or passes an EXISTING value by reference.

**⚠️ CRITICAL: Explicit sharing, NOT automatic interning**

The Rust implementation must **explicitly mirror OCaml's allocation patterns**. Do NOT try to automatically intern or deduplicate values based on content. Instead:

1. **Study the OCaml code** to understand when it passes a reference vs. constructs a new value
2. **Explicitly share** in Rust when OCaml passes the same reference (use the same arena index)
3. **Explicitly allocate new** in Rust when OCaml constructs a new value (push a new arena entry)

This means the Rust parser code must be structured to match OCaml's data flow:
- If OCaml binds a value to a variable and passes that variable to multiple places, Rust should do the same with an arena index
- If OCaml constructs a new record/value at each use site, Rust should push a new arena entry each time

**Do NOT use content-based deduplication** (e.g., "if we've seen this string before, reuse the index"). OCaml doesn't do this—it uses pointer identity from the actual code flow. The only way to achieve parity is to replicate the exact same allocation patterns.

**⚠️ STRING SHARING: Use StrIdx, NOT content-based interning**

For strings in the AST (identifiers, labels, file names), use `StrIdx` arena indices to control sharing:
- **Same StrIdx = same object** - Will be shared in marshal output via back-references
- **Different StrIdx = different objects** - Even if content is identical, will NOT be shared
- **Do NOT intern strings automatically** - Don't use hash-based deduplication in the marshal writer
- **Pass StrIdx by reference** - When OCaml passes the same string reference, use the same StrIdx in Rust

Example: For punned labeled arguments like `~compare`, the label and identifier should use the SAME `StrIdx` because OCaml reuses the same string pointer. Create the StrIdx once and pass it to both the `ArgLabel` and the `Longident`.

To achieve binary parity, you MUST study the OCaml sources (tokenizer, parser, marshaller) to understand:
1. **When does OCaml construct NEW values?** - Look for `{...}` record literals, `::` cons operations, function calls that return new allocations
2. **When does OCaml pass EXISTING values by reference?** - Look for variables being passed directly, pattern match bindings that propagate references

Examples of what to look for in OCaml:
```ocaml
(* NEW allocation - each call creates a fresh string *)
let lid = Longident.Lident name in ...

(* SAME reference - passing the same variable *)
let loc = start_pos in
{ pexp_loc = loc; ... }  (* loc is the SAME object as start_pos *)

(* NEW allocation - record literal creates new record *)
{ txt = s; loc = l }  (* This is always a new allocation *)
```

The Rust implementation must replicate these EXACT allocation patterns using the arena:
- When OCaml creates a new value, Rust should push a new entry to the arena
- When OCaml passes the same value, Rust should reuse the same arena index
- The arena index becomes the "pointer identity" for marshalling purposes

**Do NOT assume values are always interned.** Some values may be shared in certain contexts but not others, depending on how the OCaml code flows. Always trace the exact code path in the OCaml implementation.

**⚠️ CORE PRINCIPLE: Replicate OCaml's memory allocation, not content**

OCaml constructs new values OR passes by reference. The Marshal implementation shares values according to how they were allocated by memory management—NOT based on content equality. We do NOT need content-based interning because that's not how OCaml works. Instead:

1. **Study the OCaml source code** - Look at how values are constructed and passed. If a value is bound to a variable and that variable is passed to multiple places, those are all the same reference.

2. **Create an arena for types that need sharing** - If the OCaml code shares values of a particular type (e.g., `Location.t`, `Longident.t`), ensure we have an arena for that type in `ParseArena`.

3. **Pass by arena index, not by value** - When the OCaml code passes a reference, use the same arena index in Rust. Do NOT push a new value into the arena—reuse the existing index.

4. **Push new values only when OCaml allocates** - When OCaml constructs a fresh value (record literal, function call returning new allocation), push a new entry to the arena.

If we do this consistently—replicating OCaml's exact allocation and reference patterns—the binary AST WILL match byte-for-byte.
