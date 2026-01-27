# Task: Complete Parser Missing Syntax Support

## Objective

Complete the Rust ReScript parser to handle all ReScript syntax and eliminate parser-caused failures in the syntax test corpus.

**Status (2026-01-18)** (measured on `tests/syntax_tests/data`, 5s timeout per file):

- Total `.res` files: 1233
- Idempotent roundtrip (parse→print→parse): 481
- `PARSE_FAIL` (initial parse): 662
- `ROUNDTRIP_PARSE_FAIL` (re-parse printed output): 79
- `ROUNDTRIP_DIFF` (printer idempotency; tracked in TASK_PRINTER_ROUNDTRIP.md): 11
- Timeouts: 0

**Goal**: drive `PARSE_FAIL` and `ROUNDTRIP_PARSE_FAIL` to 0.

## Concurrency Note

**This task can be worked on concurrently with TASK_PRINTER_ROUNDTRIP.md**

- This task modifies: `expr.rs`, `pattern.rs`, `typ.rs`, `module.rs`, `core.rs`, `scanner.rs`
- The printer task modifies: `printer.rs` only
- No file conflicts - these are independent workstreams

## Context

- **Parser location**: `compiler-rust/src/parser/`
- **OCaml reference**: `compiler/syntax/src/res_core.ml` (~7365 lines)
- **Test command**: Run from project root (use a 5s timeout per file):
  ```bash
  ./compiler-rust/target/release/res_parser_rust -print res <file>
  ```

## Missing Features (by priority)

Counts below are derived from the **first diagnostic line** of each `PARSE_FAIL` file (so they’re approximate, but good for prioritization).

### HIGH PRIORITY (most `PARSE_FAIL`)

1. **Module Expressions** (85)
   - `include M` - module include
   - `open M` - module open
   - `module F = (X: S) => ...` - functors
   - `module type S = { ... }` - module types
   - Reference: `res_core.ml` `parseModuleExpr`, `parseModuleType`

2. **Division Operator** (27)
   - `/` conflicts with regex and comments
   - Need context-aware tokenization
   - Reference: `res_scanner.ml` handles this via mode

3. **Object/Record Field Syntax** (27)
   - `{"field": value}` - JS object syntax
   - `{...spread}` - spread in records
   - `{field}` - punned fields
   - Reference: `res_core.ml` `parseRecordExpr`

4. **Identifier Edge Cases** (18)
   - Unexpected identifier forms / module identifiers
   - Reference: `res_core.ml` identifier parsing utilities + `res_scanner.ml`

5. **Braced/Block Expressions** (15)
   - `{ let x = 1; x + 1 }` - block with statements (and block-ish forms in expression positions)
   - Reference: `res_core.ml` `parseBlockExpr`

### MEDIUM PRIORITY (still common)

6. **Colon Contexts (constraints/annotations)** (13)
   - `[1 :int, 2 :int]` - constrained array elements
   - `(expr : type)` - parenthesized constraints
   - `(pat : type)` - constrained patterns
   - Reference: `res_core.ml` `parseConstrainedExpr`

7. **Angle Bracket / Type Parameter Contexts** (10)
   - `<T>` parsing context + `>` recovery (often cascades)
   - Reference: `res_core.ml` generic/type-param parsing + `res_scanner.ml` diamond mode

8. **Module Types** (≥7)
   - `module type S = ...`
   - `module type of ...`
   - Reference: `res_core.ml` `parseModuleType`

9. **Labeled Arguments in Types** (7)
   - `(~foo: int) => unit`
   - Reference: `res_core.ml` type arrow parsing

10. **And Keyword** (6)
   - `type t = int and s = string` - mutually recursive types
   - `let rec f = ... and g = ...` - mutually recursive lets
   - Reference: `res_core.ml` `parseTypeDefinitions`

### LOW PRIORITY (edge cases)

11. **Underscore Sugar** (7) - `_` placeholder
12. **Type Extensions** (4) - `type t += Constructor` (`+=`)
13. **Single-quote Type Vars** (4) - `'a`
14. **Backtick Patterns** (4) - `` `Variant `` in patterns
15. **Tagged Templates** (3) - `` sql`SELECT *` ``
16. **Exotic Identifiers** - `\\#exotic` identifiers

## Implementation Strategy

1. **Start with module expressions** - currently the largest single `PARSE_FAIL` bucket

2. **Compare with OCaml parser** - For each feature:
   ```bash
   # See how OCaml parses a file
   _build/install/default/bin/res_parser -print sexp <file>
   ```

3. **Add tests with timeouts**
   - Prefer adding **parse-only** tests next to the parser code you changed (`expr.rs`, `module.rs`, `typ.rs`, …).
   - If the printer already supports the syntax, also add a **roundtrip** test in `printer.rs` to lock in end-to-end behavior.

   Example roundtrip tests (in `printer.rs`):
   ```rust
   #[test]
   fn test_module_exprs() {
       roundtrip("module M = { let x = 1 }");
       roundtrip("open M");
       roundtrip("include M");
   }
   ```

4. **Use 5-second timeouts** - Parser can hang on malformed input or bad recovery:
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
# Full corpus classification: PARSE_FAIL vs ROUNDTRIP_PARSE_FAIL vs DIFF
P="./compiler-rust/target/release/res_parser_rust"
tmp1="$(mktemp -t r1.XXXXXX.res)"
tmp2="$(mktemp -t r2.XXXXXX.res)"
parse_fail=0; reparse_fail=0; diff_fail=0; ok=0; total=0
while IFS= read -r -d '' f; do
  total=$((total+1))
  if ! timeout 5 "$P" --print res "$f" >"$tmp1" 2>/dev/null; then
    parse_fail=$((parse_fail+1)); continue
  fi
  if ! timeout 5 "$P" --print res "$tmp1" >"$tmp2" 2>/dev/null; then
    reparse_fail=$((reparse_fail+1)); continue
  fi
  diff -q "$tmp1" "$tmp2" >/dev/null && ok=$((ok+1)) || diff_fail=$((diff_fail+1))
done < <(find tests/syntax_tests/data -name '*.res' -print0)
rm -f "$tmp1" "$tmp2"
echo "total=$total ok=$ok parse_fail=$parse_fail reparse_fail=$reparse_fail diff_fail=$diff_fail"
```

## Test File Identification

To find files that fail parsing (your scope):
```bash
# List files that fail to parse (PARSE_FAIL)
P="./compiler-rust/target/release/res_parser_rust"
while IFS= read -r -d '' f; do
  timeout 5 "$P" --print res "$f" >/dev/null 2>&1 || echo "$f"
done < <(find tests/syntax_tests/data -name '*.res' -print0)
```

Files that parse successfully but have roundtrip issues are handled by the printer task.

## Success Criteria

- [ ] `PARSE_FAIL` reduced from 662 to <100
- [ ] All HIGH priority features implemented
- [ ] `ROUNDTRIP_PARSE_FAIL` reduced from 79 to 0
- [ ] All new parser features have tests with timeouts
- [ ] `cargo test` passes
- [ ] No new timeouts or hangs
