# Comprehensive Binary AST Parity Plan

## Executive Summary

This document provides a deep technical analysis of OCaml's Marshal format, object allocation model, and the specific sharing patterns that must be replicated in the Rust compiler to achieve byte-for-byte identical binary AST output.

**Current Status:**
- Sexp parity: 100% (1049/1049 tests pass) - AST structure is correct
- Binary parity: NOT achieved - sharing patterns differ

**Root Cause:** OCaml's Marshal uses **pointer-based object sharing**, while the Rust implementation uses a hybrid of content-based and identity-based sharing that doesn't exactly match OCaml's behavior.

---

## Part 1: OCaml's Object Model and Marshal Semantics

### 1.1 OCaml Memory Model

OCaml values are either:
1. **Immediate values** (integers, booleans, unit) - encoded directly in 63 bits
2. **Heap-allocated blocks** (records, tuples, variants with data, strings) - referenced by pointers

When you write in OCaml:
```ocaml
let pos1 = {pos_fname = "test.res"; pos_lnum = 1; pos_bol = 0; pos_cnum = 5}
let pos2 = pos1  (* pos2 points to SAME memory object as pos1 *)
```

The variables `pos1` and `pos2` hold the **same pointer**. They are physically identical (`pos1 == pos2` is true).

When you write:
```ocaml
let pos1 = {pos_fname = "test.res"; pos_lnum = 1; pos_bol = 0; pos_cnum = 5}
let pos2 = {pos_fname = "test.res"; pos_lnum = 1; pos_bol = 0; pos_cnum = 5}
```

These are **two different memory allocations**. `pos1 == pos2` is false (different pointers), even though `pos1 = pos2` is true (same content).

### 1.2 Marshal's Sharing Detection

OCaml's Marshal (runtime/extern.c) maintains a hash table mapping **heap addresses** to object indices:

```c
// Pseudocode from OCaml runtime
struct extern_state {
    pos_table_t pos_table;  // Maps: pointer address → object index
    uint32_t obj_counter;   // Incremented for each NEW heap object
};

void extern_rec(value v) {
    // For heap objects (blocks, strings):
    if (extern_lookup_position(v, &pos, &h)) {
        // FOUND: This exact pointer was seen before
        uint32_t d = obj_counter - pos;  // Distance from current position
        write_shared_reference(d);       // CODE_SHARED8/16/32
        return;
    }

    // NOT FOUND: First time seeing this pointer
    write_block_or_string(v);
    record_position(v, h, obj_counter);  // Remember this pointer
    obj_counter++;
}
```

**Key insight:** Marshal shares objects based on **pointer identity**, not value equality.

### 1.3 Sharing Reference Encoding

When Marshal encounters a previously-seen pointer, it writes a **shared reference**:

| Distance | Code | Format |
|----------|------|--------|
| 1-255 | CODE_SHARED8 (0x04) | 1 byte offset |
| 256-65535 | CODE_SHARED16 (0x05) | 2 bytes big-endian offset |
| 65536+ | CODE_SHARED32 (0x06) | 4 bytes big-endian offset |

The **distance** is `current_obj_counter - position_when_first_written`.

Example:
```
Object 0: Position {line=1, cnum=0}  ← Written fully
Object 1: Location {start=ref(0), end=...}  ← start references object 0
Object 2: Location {start=ref(0), ...}  ← start also references object 0, distance=2
```

---

## Part 2: Position Allocation in the ReScript Parser

### 2.1 Scanner Position Creation

In `compiler/syntax/src/res_scanner.ml` (lines 42-54):

```ocaml
let position scanner =
  Lexing.{
    pos_fname = scanner.filename;
    pos_lnum = scanner.lnum;
    pos_bol = scanner.line_offset;
    pos_cnum = scanner.line_offset + scanner.offset16;
  }
```

**Critical behavior:** Each call to `position scanner` allocates a **NEW** record on the heap with a **unique pointer address**.

The `scan` function (approximately):
```ocaml
let scan scanner =
  let start_pos = position scanner in  (* NEW allocation *)
  (* ... scan token ... *)
  let end_pos = position scanner in    (* ANOTHER NEW allocation *)
  (start_pos, end_pos, token)
```

### 2.2 Parser Position Assignment

In `compiler/syntax/src/res_parser.ml` (lines 65-95):

```ocaml
let rec next ?prev_end_pos p =
  let prev_end_pos =
    match prev_end_pos with
    | Some pos -> pos           (* Use explicitly provided position *)
    | None -> p.end_pos         (* REUSE existing end_pos object! *)
  in
  let start_pos, end_pos, token = Scanner.scan p.scanner in
  (* start_pos and end_pos are NEW objects from scanner *)

  match token with
  | Comment c ->
      Comment.set_prev_tok_end_pos c p.end_pos;  (* Store reference to SAME object *)
      p.comments <- c :: p.comments;
      p.prev_end_pos <- p.end_pos;  (* SAME object reference *)
      p.end_pos <- end_pos;         (* NEW object from scanner *)
      next ~prev_end_pos p          (* Recurse with old prev_end_pos *)
  | _ ->
      p.token <- token;
      p.prev_end_pos <- prev_end_pos;  (* SAME object as previous end_pos *)
      p.start_pos <- start_pos;        (* NEW object from scanner *)
      p.end_pos <- end_pos             (* NEW object from scanner *)
```

### 2.3 The Critical Sharing Pattern

When parsing `let x = 1`:

```
Token 1: "let" at positions 0-3
  Scanner creates: start_pos_A (id=1), end_pos_A (id=2)
  Parser assigns: p.start_pos ← start_pos_A, p.end_pos ← end_pos_A

Token 2: "x" at positions 4-5
  Scanner creates: start_pos_B (id=3), end_pos_B (id=4)
  Parser assigns:
    prev_end_pos ← p.end_pos (which is end_pos_A, id=2!)  ← SHARING HERE
    p.start_pos ← start_pos_B
    p.end_pos ← end_pos_B
    p.prev_end_pos ← prev_end_pos (which is end_pos_A)
```

**Result:** The position object `end_pos_A` is now referenced by BOTH:
- Token 1's end position
- Token 2's `prev_end_pos`

When these locations are marshaled, OCaml detects they share the same pointer and writes a `CODE_SHARED*` reference.

### 2.4 Position Sharing Summary

| Source | Creates New Object? | Shared With |
|--------|---------------------|-------------|
| Scanner.scan start_pos | YES (new pointer) | Nothing (unique) |
| Scanner.scan end_pos | YES (new pointer) | Next token's prev_end_pos |
| prev_end_pos assignment | NO (reuses pointer) | Previous token's end_pos |
| Comment.set_prev_tok_end_pos | NO (stores reference) | Current token's end_pos |
| Lexing.dummy_pos | NO (global constant) | All initial parser positions |

---

## Part 3: Current Rust Implementation Analysis

### 3.1 PositionId System

The Rust parser uses `PositionId` to track object identity:

```rust
// location.rs
pub struct PositionId(u32);

pub struct Position {
    pub file_name: String,
    pub line: i32,
    pub bol: i32,
    pub cnum: i32,
    pub id: PositionId,  // Identity marker
}
```

When a Position is **cloned**, the `id` is **copied** (same identity), which should mimic OCaml's pointer sharing.

### 3.2 Scanner Position Creation

```rust
// scanner.rs
pub fn position(&mut self) -> Position {
    let id = PositionId::from_raw(self.position_id_counter);
    self.position_id_counter += 1;  // NEW ID each time
    Position::new_with_id(&self.filename, self.lnum, ...)
}
```

Each call to `position()` creates a position with a **unique ID**. This matches OCaml's behavior where each call allocates a new record.

### 3.3 Parser Position Assignment

```rust
// state.rs
fn next_with_prev_end(&mut self, prev_end_pos: Option<Position>) {
    let prev_end = prev_end_pos.unwrap_or_else(|| self.end_pos.clone());
    //                                                      ^^^^^^
    //                               Clone preserves the PositionId!

    let result = self.scanner.scan();
    self.prev_end_pos = prev_end;        // Same ID as old end_pos
    self.start_pos = result.start_pos;   // New ID from scanner
    self.end_pos = result.end_pos;       // New ID from scanner
}
```

**Good:** The clone preserves the PositionId, which should enable identity-based sharing.

### 3.4 The Problem: Content-Based Sharing in Marshal

In `marshal.rs`:

```rust
pub fn write_position_shared(
    &mut self,
    _id: PositionId,      // ← IGNORED! Prefixed with underscore
    file_name: &str,
    line: i32,
    bol: i32,
    cnum: i32,
) -> bool {
    // Uses CONTENT, not ID!
    let content_key = (line, bol, cnum);

    if let Some(&obj_idx) = self.position_content_table.get(&content_key) {
        self.write_shared_ref(self.obj_counter - obj_idx);
        return false;
    }
    // ...
    self.position_content_table.insert(content_key, obj_idx);
    true
}
```

**Problem:** The `PositionId` is completely ignored. Sharing decisions are based on **content equality** (same line/bol/cnum), not **identity**.

### 3.5 Why Content-Based Sharing Differs

**Scenario: Two positions with same content but different identities**

```ocaml
(* OCaml *)
let pos1 = position scanner  (* Allocates NEW object, pointer P1 *)
(* ... scanner advances but returns to same location ... *)
let pos2 = position scanner  (* Allocates NEW object, pointer P2 *)
(* pos1 and pos2 have same content but different pointers *)
(* Marshal: NOT shared (different pointers) *)
```

```rust
// Rust with content-based sharing
let pos1 = scanner.position();  // id=42, content=(1, 0, 5)
// ... scanner returns to same location ...
let pos2 = scanner.position();  // id=43, content=(1, 0, 5)
// Content match! Marshal: SHARED (wrong!)
```

**Scenario: Same position assigned to two fields**

```ocaml
(* OCaml *)
let pos = position scanner  (* Pointer P1 *)
loc1.start = pos  (* References P1 *)
loc2.start = pos  (* References P1 *)
(* Marshal: SHARED (same pointer P1) *)
```

```rust
// Rust with content-based sharing
let pos = scanner.position();  // id=42
let loc1_start = pos.clone();  // id=42, content=...
let loc2_start = pos.clone();  // id=42, content=...
// Content match! Marshal: SHARED (correct, but by accident)
```

**The problem:** Content-based sharing can produce **different sharing patterns** than pointer-based sharing, depending on whether two positions with the same content were created by the same allocation or not.

---

## Part 4: The Solution

### 4.1 Switch to Identity-Based Position Sharing

Modify `marshal.rs` to use `PositionId` instead of content:

```rust
pub fn write_position_shared(
    &mut self,
    id: PositionId,  // Actually use this!
    file_name: &str,
    line: i32,
    bol: i32,
    cnum: i32,
) -> bool {
    // Check identity-based sharing FIRST
    if let Some(&obj_idx) = self.position_table.get(&id) {
        let d = self.obj_counter - obj_idx;
        self.write_shared_ref(d);
        return false;  // Shared, don't write content
    }

    // Record object index BEFORE writing (OCaml semantics)
    let obj_idx = self.obj_counter;

    // Write the position block
    self.write_block_header(0, 4);
    self.write_string_shared(file_name);
    self.write_int(line as i64);
    self.write_int(bol as i64);
    self.write_int(cnum as i64);

    // Record for future sharing by ID
    self.position_table.insert(id, obj_idx);
    true
}
```

### 4.2 Ensure Correct PositionId Assignment

The key is to ensure PositionId assignment in Rust matches OCaml's pointer assignment:

| OCaml Pattern | Rust Equivalent | PositionId Behavior |
|---------------|-----------------|---------------------|
| `pos = Scanner.position()` | `pos = scanner.position()` | New unique ID |
| `pos2 = pos` (assignment) | `pos2 = pos.clone()` | Same ID (shared) |
| `field <- pos` (mutable field) | `self.field = pos` | Move, ID preserved |
| Function return | Function return | Move, ID preserved |

### 4.3 Handle Backtracking Correctly

**Issue:** Scanner backtracking currently rewinds the position_id_counter:

```rust
pub fn restore(&mut self, snapshot: ScannerSnapshot) {
    self.position_id_counter = snapshot.position_id_counter;  // REWINDS!
}
```

This can cause ID reuse after backtracking. However, this may be acceptable because:
1. Backtracked positions are discarded (never reach final AST)
2. OCaml also allocates positions during backtracking that get garbage collected

**Recommendation:** Keep current behavior but verify through testing.

### 4.4 LocationId Sharing

Location sharing should also use identity:

```rust
impl Marshal for Location {
    fn marshal(&self, w: &mut MarshalWriter) {
        // Check if this exact Location (by ID) was seen before
        if let Some(obj_idx) = w.get_location_by_id(self.id) {
            let d = w.obj_counter() - obj_idx;
            w.write_shared_ref(d);
            return;
        }

        let obj_idx = w.obj_counter();
        w.write_block_header(0, 3);
        self.loc_start.marshal(w);  // Uses PositionId-based sharing
        self.loc_end.marshal(w);    // Uses PositionId-based sharing
        w.write_int(if self.loc_ghost { 1 } else { 0 });

        w.record_location_id(self.id, obj_idx);
    }
}
```

---

## Part 5: String Interning and Sharing

### 5.1 OCaml String Sharing in Parser

OCaml's parser uses `Token.to_string` which returns **interned string constants**:

```ocaml
let to_string = function
  | True -> "true"      (* Global constant, same pointer every time *)
  | False -> "false"    (* Global constant, same pointer every time *)
  | Plus -> "+"         (* Global constant, same pointer every time *)
  (* ... *)
```

These string literals are allocated ONCE in the program's data section. Every call to `Token.to_string True` returns the **same pointer**.

### 5.2 Rust String Interning

The current Rust implementation interns certain strings:

```rust
fn is_interned_string(s: &str) -> bool {
    // Single-character operators
    if s.len() == 1 {
        let c = s.chars().next().unwrap();
        if matches!(c, '+' | '-' | '*' | '/' | ...) {
            return true;
        }
    }
    // Keywords and multi-char operators
    matches!(s, "true" | "false" | "()" | "[]" | "==" | "!=" | ...)
}

pub fn write_identifier_string(&mut self, s: &str) {
    if Self::is_interned_string(s) {
        self.write_string_shared(s);  // Content-based sharing
    } else {
        self.write_str(s);  // No sharing
    }
}
```

**Note:** String sharing is content-based, which matches OCaml's behavior for interned strings.

### 5.3 Filename String Sharing

Filenames are shared content-based (every position has the same filename):

```rust
// In write_position_shared:
self.write_string_shared(file_name);  // Content-based, shared across all positions
```

This is correct because OCaml also shares the filename string (it's a field of the scanner, not re-allocated).

---

## Part 6: Implementation Checklist

### Phase 1: Identity-Based Position Sharing

- [ ] Modify `write_position_shared` to use `PositionId` instead of content
- [ ] Remove `position_content_table` or mark as deprecated
- [ ] Update all callers to pass the actual PositionId (not ignored)
- [ ] Add unit tests for identity-based sharing

### Phase 2: Verify Parser Position Assignment

- [ ] Audit all places where positions are assigned/cloned in the parser
- [ ] Ensure `prev_end_pos = end_pos.clone()` pattern is consistent
- [ ] Verify comments store position references correctly
- [ ] Add debug logging to trace position ID flow (temporary)

### Phase 3: Location Sharing

- [ ] Verify LocationId-based sharing is working correctly
- [ ] Remove redundant `location_table` (uses PositionIds) if `location_id_table` suffices
- [ ] Add unit tests for location sharing

### Phase 4: Integration Testing

- [ ] Create binary comparison test: `diff rust.ast ocaml.ast`
- [ ] Test on all syntax test files
- [ ] Analyze any remaining differences with hex diff
- [ ] Create minimal reproduction cases for failures

### Phase 5: Edge Cases

- [ ] Test backtracking scenarios (lookahead in parser)
- [ ] Test comment handling
- [ ] Test very large files (many positions)
- [ ] Test files with repeated identical constructs

---

## Part 7: Debugging Tools

### 7.1 Hex Diff Analysis

```bash
# Generate both outputs
./ocaml_parser -print binary test.res > /tmp/ocaml.ast
./rust_parser --print binary test.res > /tmp/rust.ast

# Compare sizes
ls -la /tmp/*.ast

# Hex diff
xxd /tmp/ocaml.ast > /tmp/ocaml.hex
xxd /tmp/rust.ast > /tmp/rust.hex
diff /tmp/ocaml.hex /tmp/rust.hex | head -100
```

### 7.2 Marshal Debug Tool

Use `scripts/debug_marshal.ml` to decode and display Marshal structure:

```bash
# Decode OCaml output
ocaml scripts/debug_marshal.ml /tmp/ocaml.ast

# Compare with Rust (need equivalent Rust tool)
```

### 7.3 Position ID Tracing

Add temporary debug output to trace position IDs:

```rust
// In scanner.rs
pub fn position(&mut self) -> Position {
    let id = self.position_id_counter;
    self.position_id_counter += 1;
    eprintln!("Scanner: new position id={} at line={} cnum={}", id, self.lnum, ...);
    // ...
}

// In state.rs
fn next_with_prev_end(&mut self, ...) {
    eprintln!("Parser: prev_end_pos id={}", prev_end.id.raw());
    eprintln!("Parser: start_pos id={}", result.start_pos.id.raw());
    eprintln!("Parser: end_pos id={}", result.end_pos.id.raw());
}
```

### 7.4 Sharing Pattern Analysis

Add debug output to marshal to see what's being shared:

```rust
// In marshal.rs
if let Some(&obj_idx) = self.position_table.get(&id) {
    eprintln!("SHARE position id={} (obj {} -> {})", id.raw(), obj_idx, self.obj_counter);
    // ...
}
```

---

## Part 8: Risk Analysis

### High Risk: Position ID Assignment Mismatch

**Risk:** Rust position IDs don't match OCaml's pointer sharing pattern exactly.

**Mitigation:**
- Comprehensive testing with all syntax test files
- Debug tracing to compare sharing decisions
- Incremental changes with frequent testing

### Medium Risk: Backtracking Changes Sharing Pattern

**Risk:** Scanner backtracking reuses position IDs, causing unexpected sharing.

**Mitigation:**
- Test files that trigger backtracking (e.g., complex patterns)
- May need to NOT rewind position_id_counter

### Medium Risk: String Interning Differences

**Risk:** Different set of strings interned between OCaml and Rust.

**Mitigation:**
- Analyze OCaml's Token.to_string to identify all interned strings
- Match the exact set in Rust

### Low Risk: Edge Cases in Position Calculation

**Risk:** Position values (line, bol, cnum) differ due to UTF-16 calculation bugs.

**Mitigation:**
- Already passing sexp parity (position VALUES are correct)
- This affects content, not sharing

---

## Part 9: Success Criteria

1. **Binary size match:** Rust and OCaml outputs have identical byte counts
2. **Object count match:** Marshal header shows same obj_count
3. **Byte-for-byte identical:** `diff rust.ast ocaml.ast` produces no output
4. **All syntax tests pass:** 100% of test files produce identical binary
5. **No sexp regression:** Sexp parity remains at 100%

---

## Part 10: References

### OCaml Sources
- `runtime/extern.c` - Marshal serialization
- `runtime/caml/intext.h` - Marshal format constants
- `runtime/intern.c` - Marshal deserialization

### ReScript OCaml Sources
- `compiler/syntax/src/res_scanner.ml` - Position allocation
- `compiler/syntax/src/res_parser.ml` - Position assignment
- `compiler/depends/binary_ast.ml` - Binary AST writing

### Rust Implementation
- `compiler-rust/src/location.rs` - Position/Location types
- `compiler-rust/src/parser/scanner.rs` - Position creation
- `compiler-rust/src/parser/state.rs` - Parser position tracking
- `compiler-rust/src/binary_ast/marshal.rs` - Marshal serialization

---

## Appendix A: Object Counter Semantics

**Critical rule:** The object counter is incremented AFTER writing each heap object.

```
Initial: obj_counter = 0

Write block (tag=0, size=2):
  1. Write header byte 0xA0
  2. obj_counter is STILL 0 (this object gets index 0)
  3. Write field 0
  4. Write field 1
  5. Increment: obj_counter = 1

Write string "hello":
  1. Write header byte 0x25
  2. Write "hello"
  3. obj_counter is STILL 1 (this string gets index 1)
  4. Increment: obj_counter = 2

Write shared reference to string (index 1):
  1. Distance = obj_counter - 1 = 2 - 1 = 1
  2. Write CODE_SHARED8, 0x01
  3. obj_counter stays 2 (shared refs don't increment!)
```

---

## Appendix B: Complete Sharing Table Structure

The MarshalWriter should have:

```rust
pub struct MarshalWriter {
    buffer: Vec<u8>,
    obj_counter: u32,
    size_32: u32,
    size_64: u32,

    // Identity-based sharing (primary for AST parity)
    position_table: HashMap<PositionId, u32>,       // Position identity → obj index
    location_id_table: HashMap<LocationId, u32>,    // Location identity → obj index

    // Content-based sharing (for strings and specific cases)
    string_table: HashMap<String, u32>,             // String content → obj index

    // DEPRECATED: Remove after switching to identity-based
    position_content_table: HashMap<(i32, i32, i32), u32>,
    location_content_table: HashMap<(...), u32>,
    location_table: HashMap<LocationKey, u32>,      // Redundant with location_id_table
}
```

---

## Appendix C: Test Matrix

| Test Category | Test File Pattern | Expected Behavior |
|---------------|-------------------|-------------------|
| Simple binding | `let x = 1` | Position sharing: prev_end_pos shared |
| Multiple bindings | `let a = 1; let b = 2` | Each binding has unique positions |
| Nested expressions | `let x = f(a, b, c)` | Many positions, complex sharing |
| Comments | `let x = 1 (* comment *)` | Comment stores position reference |
| Backtracking | Complex patterns | IDs may be non-sequential |
| Large file | 1000+ tokens | Many shared references |
| Repeated constructs | `let a = 1; let b = 1` | Same content, different IDs |

---

*Document created: 2026-01-20*
*Last updated: 2026-01-20*
