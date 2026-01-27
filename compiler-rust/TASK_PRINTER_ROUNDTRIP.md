# Task: Fix Printer Roundtrip Issues

## Objective

Fix the Rust ReScript printer to produce output that is idempotent (parse→print→parse→print produces identical output). Focus on **ROUNDTRIP_DIFF** cases where the printer output changes between passes.

## Concurrency Note

**This task can be worked on concurrently with TASK_PARSER_FEATURES.md**

- This task modifies: `printer.rs` only
- The parser task modifies: `expr.rs`, `pattern.rs`, `typ.rs`, `module.rs`, `core.rs`, `scanner.rs`
- No file conflicts - these are independent workstreams

**Scope limitation**: This task focuses on **ROUNDTRIP_DIFF** cases (pure printer bugs). The **ROUNDTRIP_PARSE_FAIL** cases depend on parser improvements and will be resolved by the parser task.

## Context

- **Printer location**: `compiler-rust/src/parser/printer.rs`
- **OCaml reference**: `compiler/syntax/src/res_printer.ml`
- **Test command**: Compare two roundtrips:
  ```bash
  P="./compiler-rust/target/release/res_parser_rust"
  $P -print res file.res > /tmp/r1.res
  $P -print res /tmp/r1.res > /tmp/r2.res
  diff /tmp/r1.res /tmp/r2.res
  ```

## Roundtrip Failure Categories

### ROUNDTRIP_PARSE_FAIL (OUT OF SCOPE - handled by parser task)

These are blocked by missing parser features:

1. **Module-related syntax** - printer emits valid ReScript that parser doesn't support yet
2. **Type annotations** - printer emits types that parser can't parse yet
3. **JSX** - printer emits JSX that parser can't handle yet

**Do not try to fix these** - they will be resolved when the parser task completes.

### ROUNDTRIP_DIFF (IN SCOPE - pure printer bugs)

These indicate the printer isn't idempotent:

1. **Attribute Parentheses** (most common)
   ```rescript
   // Input
   let x = @attr (a + b)

   // After 1st print (WRONG - loses parens)
   let x = @attr a + b

   // After 2nd print
   let x = @attr a + b
   ```
   **Fix**: Check if attributed expression needs parentheses for precedence

2. **Type Chaining with And**
   ```rescript
   // Input
   type t = string and s = int and u = float

   // After 1st print (WRONG - loses 3rd type's 'and')
   type t = string
   and s = int

   // Missing: and u = float
   ```
   **Fix**: `print_type_definitions` needs to handle >2 mutually recursive types

3. **Long Tuple Formatting**
   ```rescript
   // Input (formatted with line breaks)
   type t = (
     superLongTypeName,
     superLongTypeName,
   )

   // After print (WRONG - all on one line)
   type t = (superLongTypeName, superLongTypeName)
   ```
   **Fix**: Add line width tracking and break long tuples

4. **Type Constructor Parentheses**
   ```rescript
   // Input
   type t = (string, int) constr

   // After 1st print (WRONG - loses outer parens)
   type t = string, int constr
   ```
   **Fix**: Parenthesize tuple arguments to type constructors

5. **Comment Preservation**
   - Comments attached to wrong nodes
   - Comments lost entirely
   **Fix**: Track and emit leading/trailing comments on each AST node

## Implementation Strategy

1. **Start with attribute parentheses** - most common issue, clear fix

2. **Add specific roundtrip tests** for each bug:
   ```rust
   #[test]
   fn test_attr_parens_preserved() {
       let input = "let x = @attr (a + b)";
       let r1 = roundtrip(input);
       let r2 = roundtrip(&r1);
       assert_eq!(r1, r2, "Roundtrip not idempotent");
       assert!(r1.contains("(a + b)"), "Lost parentheses");
   }
   ```

3. **Compare with OCaml printer output**:
   ```bash
   # OCaml printer output
   _build/install/default/bin/res_parser -print res file.res

   # Rust printer output
   ./compiler-rust/target/release/res_parser_rust -print res file.res

   # Compare
   diff <(ocaml_cmd) <(rust_cmd)
   ```

4. **Use 5-second timeouts** in all tests

## Key Functions to Fix

| Function | Issue |
|----------|-------|
| `print_expression` | Attribute parentheses |
| `print_type_definitions` | And keyword for >2 types |
| `print_typ` | Tuple parentheses, line breaking |
| `print_pattern` | Pattern parentheses |
| `print_structure_item` | Comment attachment |

## Specific Files with Roundtrip Diffs

These are good test cases - they parse but have printer bugs:

```
printer/expr/binary.res          # Attribute parentheses
parsing/grammar/typedefinition/typeDefinition.res  # And keyword
printer/typexpr/tuple.res        # Tuple formatting
ast-mapping/JSXFragments.res     # JSX
conversion/reason/comments.res   # Comment preservation
```

## Test File Identification

To find ROUNDTRIP_DIFF files (your scope):
```bash
# List files with printer idempotency issues (DIFF)
P="./compiler-rust/target/release/res_parser_rust"
for f in tests/syntax_tests/data/**/*.res; do
  $P -print res "$f" > /tmp/r1.res 2>/dev/null || continue  # Skip parse failures
  $P -print res /tmp/r1.res > /tmp/r2.res 2>/dev/null || continue  # Skip re-parse failures (parser task)
  diff -q /tmp/r1.res /tmp/r2.res > /dev/null || echo "$f"  # Your scope: DIFF cases
done
```

Files that fail initial parsing are handled by the parser task.
Files where re-parsing fails (ROUNDTRIP_PARSE_FAIL) are also blocked by parser task.

## Verification

Run the full roundtrip test:
```bash
for f in tests/syntax_tests/data/**/*.res; do
  ./compiler-rust/target/release/res_parser_rust -print res "$f" > /tmp/r1.res 2>/dev/null || continue
  ./compiler-rust/target/release/res_parser_rust -print res /tmp/r1.res > /tmp/r2.res 2>/dev/null || { echo "PARSE_FAIL: $f"; continue; }
  diff -q /tmp/r1.res /tmp/r2.res > /dev/null || echo "DIFF: $f"
done | grep -c "^DIFF"
```

## Success Criteria

- [x] ROUNDTRIP_DIFF failures reduced from ~20 to 0
- [x] All fixes have dedicated tests with 5s timeouts
- [x] `cargo test` passes
- [x] Output matches OCaml printer for common cases

## Status

- ROUNDTRIP_DIFF sweep: 0 (PARSE_FAIL remains in parser task scope).
- Printer now preserves comments (best-effort ordering by location), prints type attributes, and formats bsObject/object types with `{.}` / `{..}` / quoted labels.
- Added idempotency and formatting tests in `compiler-rust/src/parser/printer.rs`.

**Note**: ROUNDTRIP_PARSE_FAIL cases will decrease as the concurrent parser task progresses - no action needed here for those.
