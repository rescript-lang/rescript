# Ralph Loop: Syntax Parity

You are working on achieving **byte-for-byte parity** between the Rust ReScript parser/printer and the OCaml reference implementation for ALL syntax tests.

## Core Principle: 1:1 OCaml Port

**This is a 1:1 rewrite of the OCaml implementation in idiomatic Rust.**

- **Study the OCaml code first** - Before writing ANY code, read and understand the corresponding OCaml implementation. Don't guess or reverse-engineer from output.
- **Match the logic exactly** - The Rust code should follow the same algorithm, control flow, and edge case handling as OCaml.
- **Use idiomatic Rust** - Translate OCaml patterns to Rust equivalents (e.g., `Option` for `option`, `match` for pattern matching, `Vec` for lists).
- **NO global state** - This is the ONLY architectural difference from OCaml. Pass all state explicitly through function parameters and return values. Use `&mut` references or context structs instead of mutable global refs.

**Reference files to study:**
| OCaml File | Purpose |
|------------|---------|
| `compiler/syntax/src/res_printer.ml` | Main printer (~6100 lines) |
| `compiler/syntax/src/res_comments_table.ml` | Comment attachment algorithm |
| `compiler/syntax/src/res_doc.ml` | Document pretty-printing |
| `compiler/syntax/src/res_parens.ml` | Parenthesization rules |

When you encounter a parity issue:
1. Find the corresponding function in the OCaml code
2. Understand what it does and WHY
3. Ensure the Rust implementation matches that logic exactly
4. Only then make changes

## Your Task

Read `RALPH_TODO.md` and complete the next unchecked task. After completing a task:
1. Mark it `[x]` complete in `RALPH_TODO.md`
2. Update the status counts at the top
3. Commit your changes with an atomic commit
4. If all tasks are complete, output "RALPH_COMPLETE"

## ⚠️ WARNING: Infinite Loop in Parser

**There is currently an infinite loop somewhere in the parsing code.** When running the parser or tests, you MUST always use timeouts to prevent stalling:

```bash
# Use timeout for all parser invocations
timeout 10s ./compiler-rust/target/release/res_parser_rust <file>

# Use timeout for test runs
timeout 120s bash -c 'PARSER=rust ./scripts/test_syntax.sh 2>&1 | tail -40'

# Use timeout for cargo build (in case of proc macro issues)
timeout 60s cargo build --manifest-path compiler-rust/Cargo.toml --release
```

If a command hangs, it likely hit the infinite loop. Identify which file triggered it and investigate the parsing logic for that construct.

## Critical Rules

1. **ALWAYS read the OCaml implementation first** - Don't guess. Study the OCaml source to understand the correct behavior. The goal is a faithful port, not a reimagining.

2. **Run tests frequently** - After each change:
   ```bash
   PARSER=rust ./scripts/test_syntax.sh 2>&1 | tail -40
   ```

3. **Commit after each fix** - Every time tests improve, commit immediately. Include the test results in the commit message:
   ```bash
   git add <files> && git commit -m "Fix: <description>

   Syntax tests: X/Y passed (Z%)

   Co-Authored-By: Claude <noreply@anthropic.com>"
   ```

   Get the current status by running:
   ```bash
   PARSER=rust ./scripts/test_syntax.sh 2>&1 | tail -20
   ```

4. **Focus on root causes** - Many failures share the same root cause (usually comment handling). Fix the underlying issue rather than individual symptoms.

5. **Never modify expected files** - The `tests/syntax_tests/data/*/expected/` files contain the OCaml reference output. Fix the Rust implementation to match them.

## Key Files

| File | Purpose |
|------|---------|
| `compiler-rust/src/parser/printer2.rs` | Main printer implementation |
| `compiler-rust/src/parser/comment_table.rs` | Comment attachment logic |
| `compiler-rust/src/parser/ml_printer.rs` | ML AST printer (for `-print ml`) |
| `compiler-rust/src/parser/diagnostics.rs` | Error messages and recovery |
| `compiler/syntax/src/res_printer.ml` | OCaml reference (6100 lines) |
| `compiler/syntax/src/res_comments_table.ml` | OCaml comment handling |
| `compiler/syntax/src/res_diagnostics.ml` | OCaml error messages |
| `compiler/syntax/src/res_outcome_printer.ml` | OCaml ML printer |

## Debugging Workflow

```bash
# View diff for a specific failing test
diff tests/syntax_tests/data/printer/comments/expected/FILE.txt \
     tests/temp/syntax_tests/data/printer/comments/expected/FILE.txt

# Test a single file (printer mode - default)
./compiler-rust/target/release/res_parser_rust tests/syntax_tests/data/printer/FILE.res

# Test with error recovery mode (for parsing/errors tests)
./compiler-rust/target/release/res_parser_rust -recover -print ml tests/syntax_tests/data/parsing/errors/FILE.res

# Test with AST conversion (for ast-mapping tests)
./compiler-rust/target/release/res_parser_rust -test-ast-conversion -jsx-version 4 tests/syntax_tests/data/ast-mapping/FILE.res

# Test with JSX PPX (for ppx/react tests)
./compiler-rust/target/release/res_parser_rust -jsx-version 4 tests/syntax_tests/data/ppx/react/FILE.res

# Compare with OCaml
./_build/install/default/bin/res_parser tests/syntax_tests/data/printer/FILE.res
./_build/install/default/bin/res_parser -recover -print ml tests/syntax_tests/data/parsing/errors/FILE.res

# Build after changes
cargo build --manifest-path compiler-rust/Cargo.toml --release
```

## Common Issues to Fix

### Printer Issues
1. **Comment placement** - Comments attach to wrong AST nodes or get lost
2. **Trailing comments** - Comments after expressions not preserved
3. **Blank line preservation** - Blank lines around comments should be kept
4. **JSX in expressions** - JSX elements inside switch/match arms
5. **Functor parameter comments** - Comments on module functor parameters

### Error Recovery Issues
6. **Error message locations** - Line/column numbers in error messages must match OCaml
7. **Recovery AST output** - The recovered AST printed as ML must match exactly
8. **Error hole placement** - `[%rescript.exprhole]` placeholders for missing expressions
9. **Array access syntax** - OCaml uses `arr.(i)` syntax vs `Array.get arr i`

### PPX/AST Issues
10. **JSX transformation** - React PPX output must match OCaml's `jsx-version 4`
11. **AST conversion** - `--test-ast-conversion` output must match

## OCaml → Rust Translation Patterns

When porting OCaml code, use these Rust equivalents:

| OCaml | Rust |
|-------|------|
| `option` | `Option<T>` |
| `list` | `Vec<T>` (or `&[T]` for parameters) |
| `ref` | `&mut T` parameter or field in context struct |
| `Hashtbl.t` | `HashMap<K, V>` or field in `CommentTable` |
| `match x with` | `match x { }` |
| `let rec` | Regular `fn` (Rust has no `rec` keyword) |
| `fun x -> ...` | `\|x\| ...` or `fn` |
| `x :: xs` | `if let [x, xs @ ..] = slice` or iterator |
| `List.iter` | `.iter().for_each()` or `for` loop |
| `List.map` | `.iter().map().collect()` |
| `Printf.sprintf` | `format!()` |
| `^` (string concat) | `format!()` or `.push_str()` |

**Global state elimination example:**

```ocaml
(* OCaml with global state *)
let comments_table = Hashtbl.create 100
let attach_comment loc comment =
  Hashtbl.add comments_table loc comment
```

```rust
// Rust with explicit state
struct CommentTable {
    leading: HashMap<PosRange, Vec<Comment>>,
    trailing: HashMap<PosRange, Vec<Comment>>,
}

fn attach_comment(table: &mut CommentTable, loc: PosRange, comment: Comment) {
    table.leading.entry(loc).or_default().push(comment);
}
```

## Progress Tracking

The current status is tracked in `RALPH_TODO.md`. Update it as you complete tasks.

Run the full test suite to see current parity:
```bash
PARSER=rust ./scripts/test_syntax.sh 2>&1 | tail -30
```

The test categories are:
| Category | Description |
|----------|-------------|
| printer | Pretty-printing tests (187 files) |
| conversion | ML/Reason to ReScript conversion |
| parsing/errors | Error recovery mode tests |
| parsing/grammar | Grammar parsing tests |
| parsing/recovery | Recovery mode tests |
| parsing/infiniteLoops | Infinite loop prevention tests |
| ppx/react | JSX PPX transformation tests |
| ast-mapping | AST conversion tests |

## Exit Condition

When ALL syntax tests pass (not just printer tests), output exactly:
```
RALPH_COMPLETE
```

This triggers the Ralph plugin's `--completion-promise` to stop the loop.

**Do NOT stop when only printer tests pass.** Continue until `PARSER=rust ./scripts/test_syntax.sh` shows 0 failures across ALL categories.
