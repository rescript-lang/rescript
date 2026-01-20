# Binary AST Parity Implementation Plan

## Problem Summary

The Rust parser produces binary AST output that differs from OCaml's output. Investigation revealed:

1. **Sexp parity passes (100%)** - AST structure is correct
2. **Binary sizes match** - Same amount of sharing occurs
3. **Byte order differs** - Different sharing patterns due to position differences

## Root Cause

The Rust parser captures `start_pos` at the wrong point in several parsing functions, causing:

1. **Incorrect position values** - e.g., `pvb_loc` starts at pattern (cnum=4) instead of `let` keyword (cnum=0)
2. **Different sharing patterns** - Positions that should be unique are incorrectly shared with pattern positions

### Specific Example: `let x = 1`

| Field | OCaml | Rust | Issue |
|-------|-------|------|-------|
| pvb_loc.loc_start | cnum=0 | cnum=4 | Missing `let` keyword |
| pvb_loc.loc_end | cnum=9 | cnum=9 | OK |

## Files to Modify

### Primary Fix: `compiler-rust/src/parser/module.rs`

#### 1. `parse_let_bindings` (line 1598)

**Current:**
```rust
fn parse_let_bindings(
    p: &mut Parser<'_>,
    _rec_flag: RecFlag,
    outer_attrs: Attributes,
    unwrap: bool,
) -> Vec<ValueBinding> {
    loop {
        let start_pos = p.start_pos.clone();  // BUG: After `let` consumed
        ...
    }
}
```

**Fixed:**
```rust
fn parse_let_bindings(
    p: &mut Parser<'_>,
    start_pos: Position,  // NEW: Pass start_pos from caller
    _rec_flag: RecFlag,
    outer_attrs: Attributes,
    unwrap: bool,
) -> Vec<ValueBinding> {
    let mut binding_start = start_pos;  // Use passed start_pos for first binding
    loop {
        let attrs = parse_attributes(p);
        ...
        // Create binding with correct start position
        let loc = mk_loc(&binding_start, &p.prev_end_pos);
        ...
        // For `and` bindings, capture start BEFORE consuming `and`
        if p.token == Token::And {
            binding_start = p.start_pos.clone();
            p.next();
        } else {
            break;
        }
    }
}
```

#### 2. Callers of `parse_let_bindings`

Update all callers to pass `start_pos`:

- `parse_structure_item` (line ~150): Already captures `start_pos` at beginning ✓
- `parse_expr` for let expressions: Need to verify
- Any other callers

### Similar Patterns to Check

Search for other functions that may have the same issue:

```rust
// Pattern to look for:
fn parse_X(...) {
    let start_pos = p.start_pos.clone();  // If this is after token consumption, it's wrong
    ...
}
```

Likely candidates:
- `parse_module_binding`
- `parse_type_declaration`
- `parse_exception_definition`
- Expression parsing functions with leading keywords

## Implementation Steps

### Phase 1: Fix `parse_let_bindings`

1. Add `start_pos: Position` parameter to `parse_let_bindings`
2. Update function to use passed `start_pos` for first binding
3. For `and` bindings, capture `start_pos` BEFORE consuming `and`
4. Update all callers to pass `start_pos`
5. Test: `let x = 1` should have pvb_loc from cnum=0

### Phase 2: Audit Similar Functions

1. List all parsing functions with leading keywords
2. For each, verify start_pos is captured BEFORE keyword consumption
3. Fix any that capture start_pos too late

### Phase 3: Comprehensive Testing

1. Run binary parity test on all syntax test files
2. Compare position values (not just structure)
3. Verify sharing patterns match

## Testing Strategy

### Unit Test for Position Correctness

```rust
#[test]
fn test_value_binding_location() {
    let structure = parse("let x = 1");
    let item = &structure[0];
    // pvb_loc should start at 0 (before `let`), not 4 (at `x`)
    assert_eq!(item.pstr_loc.loc_start.cnum, 0);
    assert_eq!(item.pstr_loc.loc_end.cnum, 9);
}
```

### Binary Parity Test

```bash
# Should produce identical output
ocaml_parser -print binary test.res > ocaml.ast
rust_parser -print binary test.res > rust.ast
diff ocaml.ast rust.ast  # Should be empty
```

## Success Criteria

1. All syntax test files produce byte-identical binary AST
2. Position values match OCaml exactly
3. Sharing patterns match (same objects shared)
4. No regressions in sexp parity

## Notes

### Why Sexp Parity Passed

The sexp format doesn't include exact position values, only AST structure. Two ASTs can have:
- Identical sexp output
- Different position values in binary

### OCaml Position Sharing Pattern

In OCaml:
```ocaml
let start_pos = p.start_pos in  (* Reference to position object *)
```

When `p.start_pos` is reassigned to a new position, `start_pos` still refers to the OLD position object. This is pointer semantics.

In Rust:
```rust
let start_pos = p.start_pos.clone();  // Copy with same PositionId
```

Clone preserves PositionId, making it equivalent to OCaml's pointer semantics for sharing purposes. The issue is WHEN we clone, not HOW.

### Position Flow Through Parser

```
parse_structure_item:
  start_pos = p.start_pos.clone()   // Capture BEFORE any token consumption
  attrs = parse_attributes(p)        // May advance parser
  match p.token {
    Token::Let => {
      p.next()                        // Consumes `let`
      parse_let_bindings(p, start_pos, ...)  // Pass captured start_pos
    }
  }
```
