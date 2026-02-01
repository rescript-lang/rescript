# Ralph Loop: Printing Parity

You are working on achieving **byte-for-byte printing parity** between the Rust ReScript printer and the OCaml reference implementation.

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

## Critical Rules

1. **ALWAYS read the OCaml implementation first** - Don't guess. Study the OCaml source to understand the correct behavior. The goal is a faithful port, not a reimagining.

2. **Run tests frequently** - After each change:
   ```bash
   PARSER=rust ./scripts/test_syntax.sh 2>&1 | tail -40
   ```

3. **Commit after each fix** - Every time tests improve, commit immediately. Include the new parity in the commit message:
   ```bash
   git add <files> && git commit -m "Fix: <description>

   Printer parity: X/187 (Y%)

   Co-Authored-By: Claude <noreply@anthropic.com>"
   ```

   Get the current parity by running:
   ```bash
   PARSER=rust ./scripts/test_syntax.sh 2>&1 | grep "^printer"
   ```

4. **Focus on root causes** - Many failures share the same root cause (usually comment handling). Fix the underlying issue rather than individual symptoms.

5. **Never modify expected files** - The `tests/syntax_tests/data/*/expected/` files contain the OCaml reference output. Fix the Rust implementation to match them.

## Key Files

| File | Purpose |
|------|---------|
| `compiler-rust/src/parser/printer2.rs` | Main printer implementation |
| `compiler-rust/src/parser/comment_table.rs` | Comment attachment logic |
| `compiler/syntax/src/res_printer.ml` | OCaml reference (6100 lines) |
| `compiler/syntax/src/res_comments_table.ml` | OCaml comment handling |

## Debugging Workflow

```bash
# View diff for a specific failing test
diff tests/syntax_tests/data/printer/comments/expected/FILE.txt \
     tests/temp/syntax_tests/data/printer/comments/expected/FILE.txt

# Test a single file
./compiler-rust/target/release/res_parser_rust tests/syntax_tests/data/printer/FILE.res

# Compare with OCaml
./_build/install/default/bin/res_parser tests/syntax_tests/data/printer/FILE.res

# Build after changes
cargo build --manifest-path compiler-rust/Cargo.toml --release
```

## Common Issues to Fix

1. **Comment placement** - Comments attach to wrong AST nodes or get lost
2. **Trailing comments** - Comments after expressions not preserved
3. **Blank line preservation** - Blank lines around comments should be kept
4. **JSX in expressions** - JSX elements inside switch/match arms
5. **Functor parameter comments** - Comments on module functor parameters

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

When all printer tests pass (187/187), move on to other categories.

## Exit Condition

When all tasks in `RALPH_TODO.md` are checked `[x]`, output exactly:
```
RALPH_COMPLETE
```

This triggers the Ralph plugin's `--completion-promise` to stop the loop.
