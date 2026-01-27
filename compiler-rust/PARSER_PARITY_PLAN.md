# Parser Parity Master Plan

## Goal

Achieve **100% parse tree parity** between the Rust and OCaml ReScript parsers. This means:
1. Identical AST structure
2. Identical source locations on all nodes
3. Identical values for ALL fields in the parsetree

## Approach: Complete S-expression Serialization

### Why S-expressions?

We use S-expressions (`sexp-locs`) as the comparison format because:

| Criterion | S-expression | JSON | Binary |
|-----------|--------------|------|--------|
| Human readable | ✅ Excellent | ⚠️ Verbose | ❌ No |
| Easy to diff | ✅ Great | ⚠️ Noisy | ❌ Hard |
| Matches AST structure | ✅ Perfect | ⚠️ Objects don't map well | N/A |
| Both OCaml/Rust support | ✅ Yes | Would need new | N/A |

### Complete Field Serialization

The `sexp-locs` format now includes **ALL** fields from the parsetree, not just the commonly used ones. This ensures we catch any parsing differences.

#### Fields Added (2026-01-26)

The following fields were previously omitted but are now serialized:

| AST Node | Field | Type | Purpose |
|----------|-------|------|---------|
| `core_type` | `ptyp_attributes` | `attributes` | Attributes on type expressions |
| `pattern` | `ppat_attributes` | `attributes` | Attributes on patterns |
| `expression` | `pexp_attributes` | `attributes` | Attributes on expressions |
| `Ptyp_arrow` | `arity` | `int option` | Function arity hint |
| `Ptyp_arrow` | `arg.attrs` | `attributes` | Attributes on arrow argument |
| `Pexp_fun` | `arity` | `int option` | Function arity hint |
| `Pexp_fun` | `async` | `bool` | Whether function is async |
| `Pexp_apply` | `partial` | `bool` | Partial application marker |
| `Pexp_apply` | `transformed_jsx` | `bool` | JSX transformation marker |
| `case` | `pc_bar` | `position option` | Position of the `\|` bar |
| `record_element` (pattern) | `opt` | `bool` | Optional field marker |
| `record_element` (expression) | `opt` | `bool` | Optional field marker |
| `label_declaration` | `pld_optional` | `bool` | Optional record field |
| `open_description` | `popen_override` | `override_flag` | Override flag on opens |

#### Implementation Files

- **OCaml**: `compiler/syntax/src/res_ast_debugger.ml` (module `SexpAstWithLocs`)
- **Rust**: `compiler-rust/src/parser/sexp_locs.rs`

---

## Testing Strategy

### Three-Level Parity Testing

```
Level 1: sexp (structure only)     - Basic AST structure
Level 2: sexp-locs (+ locations)   - Structure + all locations
Level 3: Binary AST (byte parity)  - Exact byte-for-byte match
```

### Running Tests

```bash
# Run all syntax tests with Rust parser
PARSER=rust ./scripts/test_syntax.sh

# Run AST parity tests
./scripts/test_parser_ast_parity.sh

# Compare single file
diff <(./_build/install/default/bin/res_parser -print sexp-locs test.res) \
     <(./compiler-rust/target/release/res_parser_rust -print sexp-locs test.res)
```

### Workflow for Fixing Parity Issues

1. **Identify failing file**
   ```bash
   grep '^FAIL:' tests/temp/results.txt | head -1
   ```

2. **View the difference**
   ```bash
   diff <(./_build/install/default/bin/res_parser -print sexp-locs file.res) \
        <(./compiler-rust/target/release/res_parser_rust -print sexp-locs file.res)
   ```

3. **Identify the discrepancy** - Which node has wrong value/location?

4. **Fix in Rust parser** - Usually in `parser/expr.rs`, `parser/pattern.rs`, `parser/typ.rs`, or `parser/module.rs`

5. **Verify fix**
   ```bash
   cargo test && PARSER=rust ./scripts/test_syntax.sh
   ```

---

## Phase 1: Location Parity (Current Focus)

### Current Status
- Structure parity: ✅ 100%
- Location parity: ~23% (estimated 631 files with differences)

### Known Location Issues

From PROGRESS.md analysis:
- `Ptyp_var` location
- `Ptyp_constr` location
- `value_description` location
- `Pmod_constraint` location
- `Ptyp_arrow` location
- `Pexp_construct` (::) location

### Approach

1. Categorize location differences by AST node type
2. Fix each category systematically
3. Track progress in PROGRESS.md

---

## Phase 2: Semantic Field Parity

### Fields to Verify

Now that all fields are serialized, verify these match:

- [ ] `arity` on `Pexp_fun` and `Ptyp_arrow`
- [ ] `async` on `Pexp_fun`
- [ ] `partial` on `Pexp_apply`
- [ ] `transformed_jsx` on `Pexp_apply`
- [ ] `pc_bar` on `case`
- [ ] `opt` on record fields
- [ ] `pld_optional` on label declarations
- [ ] `popen_override` on open descriptions
- [ ] All `attributes` on types, patterns, expressions

---

## Phase 2.5: Parsetree ↔ Parsetree0 Roundtrip

### Background

ReScript maintains two AST versions:
- **parsetree**: The current internal AST used by the compiler
- **parsetree0**: A frozen PPX-compatible AST for external tools

PPX tools work with parsetree0, so we must convert:
1. parsetree → parsetree0 (via `ast_mapper_to0.ml` / `mapper_to0.rs`)
2. parsetree0 → parsetree (via `ast_mapper_from0.ml` / `mapper_from0.rs`)

### Current Testing Status

The `-test-ast-conversion` flag in both parsers performs this roundtrip:

```bash
# Test roundtrip with OCaml
./_build/install/default/bin/res_parser -test-ast-conversion file.res

# Test roundtrip with Rust
./compiler-rust/target/release/res_parser_rust -test-ast-conversion file.res
```

### What IS Preserved

**AST Structure**: The roundtrip preserves all semantic structure. Verified with:
```bash
diff <(res_parser -print sexp file.res) \
     <(res_parser -test-ast-conversion -print sexp file.res)
# Should produce no output (identical)
```

### What IS NOT Preserved

**Some Locations**: Certain locations become "ghost" locations (`-1 -1`) after roundtrip because parsetree0 doesn't store all location fields. Example:
```
Before:  (Labelled "foo" (loc 1 29 1 32))
After:   (Labelled "foo" (loc 1 -1 1 -1))
```

This is **expected behavior** - parsetree0 was designed for PPX compatibility, not perfect location preservation.

### Current Test Coverage

- **ast-mapping/** directory: Only 3 test files
- Tests use printed output (with formatting differences) rather than sexp comparison
- Tests also apply JSX transformation, conflating concerns

### Parity Verification

Both OCaml and Rust should produce identical roundtrip results:
```bash
# Verify structure parity after roundtrip
diff <(ocaml_parser -test-ast-conversion -print sexp file.res) \
     <(rust_parser -test-ast-conversion -print sexp file.res)
```

### Recommended Additional Testing

To properly test the roundtrip conversion separate from printer formatting:

```bash
# Test roundtrip preserves structure (both parsers)
for f in tests/syntax_tests/data/parsing/grammar/**/*.res; do
  diff <(res_parser -print sexp "$f") \
       <(res_parser -test-ast-conversion -print sexp "$f") || echo "DIFF: $f"
done

# Test OCaml and Rust produce same roundtrip result
for f in tests/syntax_tests/data/parsing/grammar/**/*.res; do
  diff <(ocaml -test-ast-conversion -print sexp "$f") \
       <(rust -test-ast-conversion -print sexp "$f") || echo "DIFF: $f"
done
```

---

## Phase 3: Binary AST Byte Parity

### Goal

Produce byte-identical `.ast` and `.iast` files for use by the build system and PPX tools.

### Challenge: Position Sharing

OCaml uses **pointer-based sharing** for positions (same position object = same pointer = shared in marshal output). Rust uses **content-based equality**.

### Solution

Implement `PositionId` tracking:
1. Each Position gets a unique `PositionId(u32)` when created
2. Parser preserves ID when `prev_end_pos = end_pos`
3. Marshal writer uses `PositionId` as sharing key

See PROGRESS.md for detailed implementation notes.

---

## Success Criteria

### Parser Parity Complete When:

1. **sexp-locs parity**: 100% of test files produce identical output
2. **Binary AST parity**: 100% of `.ast` files are byte-identical
3. **Print roundtrip parity**: `parse → print → parse → print` produces identical output
4. **AST conversion parity**: `parsetree → parsetree0 → parsetree` produces identical structure
5. **All tests pass**: `make test-syntax` passes with `PARSER=rust`

### Metrics to Track

| Metric | Current | Target |
|--------|---------|--------|
| sexp-locs parity | ~23% | 100% |
| Binary AST parity | 0% | 100% |
| Print roundtrip parity | ~28% | 100% |
| AST conversion parity | ✅ 100%** | 100% |

*Structure preserved; some locations intentionally become ghost locations.

**AST conversion (parsetree ↔ parsetree0) works correctly in both implementations. Any differences after conversion are due to pre-existing parsing differences (sexp parity issues), not conversion bugs.

---

## Notes

### Do NOT Modify Expected Files

**CRITICAL**: Never modify test expected files (`tests/syntax_tests/data/*/expected/`) to match Rust output. The expected files define the target behavior - fix the Rust implementation instead.

### Maintaining Both Implementations

When changing the sexp-locs format:
1. Update OCaml (`res_ast_debugger.ml` - `SexpAstWithLocs` module)
2. Update Rust (`sexp_locs.rs`)
3. Verify both produce identical output on test files
4. Update this document with any new fields

### Related Documents

- [PLAN.md](./PLAN.md) - Overall compiler rewrite plan
- [PROGRESS.md](./PROGRESS.md) - Current progress tracking
- [GLOBAL_STATE_AUDIT.md](./GLOBAL_STATE_AUDIT.md) - OCaml global state catalog
