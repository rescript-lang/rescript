# Task: Complete Parser Missing Syntax Support

## Objective

Complete the Rust ReScript parser to handle all ReScript syntax. Currently 689 files fail to parse. The goal is to reduce parse failures to zero by implementing missing syntax support.

## Concurrency Note

**This task can be worked on concurrently with TASK_PRINTER_ROUNDTRIP.md**

- This task modifies: `expr.rs`, `pattern.rs`, `typ.rs`, `module.rs`, `core.rs`, `scanner.rs`
- The printer task modifies: `printer.rs` only
- No file conflicts - these are independent workstreams

## Context

- **Parser location**: `compiler-rust/src/parser/`
- **OCaml reference**: `compiler/syntax/src/res_core.ml` (~7365 lines)
- **Test command**: Run from project root with 5s timeout per file:
  ```bash
  ./compiler-rust/target/release/res_parser_rust --print res <file>
  ```

## Missing Features (by priority)

### HIGH PRIORITY (blocks most tests)

1. **Labeled Arguments** (56 failures)
   - `~foo` - labeled argument
   - `~foo=?` - optional argument
   - `~foo as bar` - labeled with alias
   - `~foo=expr` - labeled with default
   - Reference: `res_core.ml` `parseParameter`, `parseArguments`

2. **Module Expressions** (83 failures)
   - `include M` - module include
   - `open M` - module open
   - `module F = (X: S) => ...` - functors
   - `module type S = { ... }` - module types
   - Reference: `res_core.ml` `parseModuleExpr`, `parseModuleType`

3. **Division Operator** (27 failures)
   - `/` conflicts with regex and comments
   - Need context-aware tokenization
   - Reference: `res_scanner.ml` handles this via mode

4. **Object/Record Field Syntax** (27 failures)
   - `{"field": value}` - JS object syntax
   - `{...spread}` - spread in records
   - `{field}` - punned fields
   - Reference: `res_core.ml` `parseRecordExpr`

### MEDIUM PRIORITY

5. **Type Constraints in Expressions** (24 failures)
   - `[1 :int, 2 :int]` - constrained array elements
   - `(expr : type)` - parenthesized constraints
   - Reference: `res_core.ml` `parseConstrainedExpr`

6. **Block Expressions** (15 failures)
   - `{ let x = 1; x + 1 }` - block with statements
   - Reference: `res_core.ml` `parseBlockExpr`

7. **Extension Points** (8 failures)
   - `%ext` - extension
   - `%%ext` - floating extension
   - `@attr` in complex positions
   - Reference: `res_core.ml` `parseExtension`

8. **And Keyword** (8 failures)
   - `type t = int and s = string` - mutually recursive types
   - `let rec f = ... and g = ...` - mutually recursive lets
   - Reference: `res_core.ml` `parseTypeDefinitions`

### LOW PRIORITY (edge cases)

9. **Underscore Sugar** (7 failures) - `_` placeholder
10. **Tagged Templates** (3 failures) - `` sql`SELECT *` ``
11. **Exotic Identifiers** - `\\#exotic` identifiers
12. **Backtick Patterns** (4 failures) - `` `Variant `` in patterns

## Implementation Strategy

1. **Start with labeled arguments** - highest impact, affects function definitions and calls everywhere

2. **Compare with OCaml parser** - For each feature:
   ```bash
   # See how OCaml parses a file
   _build/install/default/bin/res_parser -print sexp <file>
   ```

3. **Add tests with timeouts** - Every new feature needs tests in `printer.rs`:
   ```rust
   #[test]
   fn test_labeled_arg() {
       roundtrip("let f = (~x) => x");
       roundtrip("let f = (~x=1) => x");
       roundtrip("let f = (~x=?) => x");
   }
   ```

4. **Use 5-second timeouts** - Parser can hang on malformed input:
   ```rust
   use std::time::Duration;
   const PARSE_TIMEOUT: Duration = Duration::from_secs(5);
   ```

## Key Files to Modify

| File | Purpose |
|------|---------|
| `src/parser/expr.rs` | Expression parsing |
| `src/parser/pattern.rs` | Pattern parsing |
| `src/parser/typ.rs` | Type parsing |
| `src/parser/module.rs` | Module/structure parsing |
| `src/parser/core.rs` | Shared utilities |
| `src/parser/scanner.rs` | Division operator fix |

## Verification

After each feature, run:
```bash
# Quick check on grammar tests
for f in tests/syntax_tests/data/parsing/grammar/**/*.res; do
  timeout 5 ./compiler-rust/target/release/res_parser_rust --print res "$f" > /dev/null 2>&1 && echo "OK: $f" || echo "FAIL: $f"
done | grep -c "^OK"
```

## Test File Identification

To find files that fail parsing (your scope):
```bash
# List files that fail to parse (PARSE_FAIL)
for f in tests/syntax_tests/data/**/*.res; do
  timeout 5 ./compiler-rust/target/release/res_parser_rust --print res "$f" > /dev/null 2>&1 || echo "$f"
done
```

Files that parse successfully but have roundtrip issues are handled by the printer task.

## Success Criteria

- [ ] Parse failures reduced from 689 to <100
- [ ] All HIGH priority features implemented
- [ ] All new features have roundtrip tests with timeouts
- [ ] `cargo test` passes
- [ ] No new timeouts or hangs
