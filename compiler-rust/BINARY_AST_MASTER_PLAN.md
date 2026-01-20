# Binary AST Master Plan

This document provides a comprehensive plan for implementing byte-identical binary AST output in the Rust compiler, matching the OCaml implementation exactly.

---

## Table of Contents

1. [Executive Summary](#executive-summary)
2. [Goals and Constraints](#goals-and-constraints)
3. [Binary AST File Format](#binary-ast-file-format)
4. [OCaml Marshal Format Specification](#ocaml-marshal-format-specification)
5. [OCaml's Object Model and Sharing Semantics](#ocamls-object-model-and-sharing-semantics)
6. [Position Allocation in the ReScript Parser](#position-allocation-in-the-rescript-parser)
7. [Current Rust Implementation Analysis](#current-rust-implementation-analysis)
8. [The Solution](#the-solution)
9. [Type System Mapping](#type-system-mapping)
10. [Implementation Phases](#implementation-phases)
11. [Testing Strategy](#testing-strategy)
12. [Debugging Tools](#debugging-tools)
13. [Risk Analysis](#risk-analysis)
14. [Success Criteria](#success-criteria)
15. [References](#references)
16. [Appendices](#appendices)

---

## Executive Summary

### Current Status
- **Sexp parity: 100%** (1049/1049 tests pass) - AST structure is correct
- **Binary parity: NOT achieved** - sharing patterns differ

### Root Cause
OCaml's Marshal uses **pointer-based object sharing**, while the Rust implementation uses a hybrid of content-based and identity-based sharing that doesn't exactly match OCaml's behavior. Specifically:

1. The `write_position_shared()` function **ignores the PositionId** parameter (prefixed with `_id`)
2. Sharing decisions are based on **content equality** (same line/bol/cnum), not **identity**
3. This produces different sharing patterns than OCaml's pointer-based approach

### Why Byte-Identical Matters
1. **PPX compatibility**: Existing PPXes expect exact OCaml Marshal format
2. **Incremental builds**: Mixed Rust/OCaml compilation during migration
3. **Verification**: Easy to verify correctness by comparing outputs
4. **Drop-in replacement**: No changes needed to build tooling

---

## Goals and Constraints

### Primary Goals
- [ ] Generate `.ast` files byte-identical to OCaml output
- [ ] Generate `.iast` files byte-identical to OCaml output
- [ ] No OCaml FFI dependency
- [ ] Support all parsetree0 AST nodes
- [ ] Pass all existing tests

### Constraints
1. **No OCaml FFI**: Must be pure Rust implementation
2. **Byte-identical output**: Not just "compatible"—exact same bytes
3. **parsetree0 format**: Must use frozen PPX-compatible AST version
4. **Platform compatibility**: darwin-arm64, darwin-x64, linux-x64, win32-x64

### Non-Goals (for initial implementation)
- Reading binary AST files (can be added later)
- Supporting OCaml Marshal format for other types (only parsetree0)
- Optimizing for size or speed (correctness first)

---

## Binary AST File Format

The binary AST file has three sections:

```
┌─────────────────────────────────────────────────────────────────────┐
│ SECTION 1: Header                                                   │
├─────────────────────────────────────────────────────────────────────┤
│ Offset 0-3: Dependency section size (4 bytes, big-endian u32)       │
├─────────────────────────────────────────────────────────────────────┤
│ SECTION 2: Dependencies + Source Path                               │
├─────────────────────────────────────────────────────────────────────┤
│ Byte 4: '\n' (0x0A) - separator                                     │
│ "ModuleName1\n"                                                     │
│ "ModuleName2\n"                                                     │
│ ...                                                                 │
│ "/" - terminates deps AND starts absolute path                      │
│ "Users/path/to/source.res\n" - rest of path + newline               │
├─────────────────────────────────────────────────────────────────────┤
│ SECTION 3: OCaml Marshal Data                                       │
├─────────────────────────────────────────────────────────────────────┤
│ Magic: 0x84 0x95 0xA6 0xBE (4 bytes)                                │
│ Header: data_len(4) + obj_count(4) + size_32(4) + size_64(4)        │
│ Payload: Encoded parsetree0 AST                                     │
└─────────────────────────────────────────────────────────────────────┘
```

### Section Details

#### Section 1: Size Header (4 bytes)
```
Offset  Size  Description
0       4     Big-endian u32: size of dependency section (including leading '\n')
```

#### Section 2: Dependencies + Source Path (variable)
The dependency section contains:
1. A leading newline `\n` (0x0A)
2. Module names, each followed by `\n`
3. A `/` character that terminates deps and starts the absolute source path

**Example** (file at `/Users/dev/project/src/Demo.res` with deps `Js` and `React`):
```
00000000: 00 00 00 0c                                      # Size = 12
00000004: 0a                                               # '\n'
00000005: 4a 73 0a                                         # "Js\n"
00000008: 52 65 61 63 74 0a                                # "React\n"
0000000e: 2f                                               # "/" (end deps + start path)
0000000f: 55 73 65 72 73 2f 64 65 76 2f ...               # "Users/dev/..."
         ... 44 65 6d 6f 2e 72 65 73 0a                   # "Demo.res\n"
```

### Reference Implementation

From `compiler/depends/binary_ast.ml`:

```ocaml
let write_ast ~sourcefile ~output kind pt =
  let output_set = Ast_extract.read_parse_and_extract kind pt in
  let buf = Ext_buffer.create 1000 in
  Ext_buffer.add_char buf magic_sep_char;  (* '\n' *)
  Set_string.iter
    (fun s ->
      if s <> "" && s.[0] <> '*' then
        Ext_buffer.add_string_char buf s magic_sep_char)
    output_set;
  let oc = open_out_bin output in
  output_binary_int oc (Ext_buffer.length buf);
  Ext_buffer.output_buffer oc buf;
  output_string oc sourcefile;
  output_char oc '\n';
  output_value oc pt;  (* <-- OCaml Marshal with SHARING ENABLED *)
  close_out oc
```

**Critical finding:** Uses `output_value oc pt` **without** `No_sharing` flag, so sharing IS enabled.

---

## OCaml Marshal Format Specification

### Magic Numbers
```rust
const MAGIC_NUMBER_SMALL: u32 = 0x8495A6BE;  // Standard format (we use this)
const MAGIC_NUMBER_BIG: u32 = 0x8495A6BF;    // Large objects (>4GB)
const MAGIC_NUMBER_COMPRESSED: u32 = 0x8495A6BD;  // Compressed format
```

### Header Format (20 bytes)
```
Offset  Size  Description
0       4     Magic number (0x8495A6BE)
4       4     Data length (bytes after header)
8       4     Object count (number of objects in sharing table)
12      4     Size on 32-bit platform (words)
16      4     Size on 64-bit platform (words)
```

### Opcode Reference

#### Prefix Codes (Compact Encoding)
| Range | Binary Pattern | Description |
|-------|---------------|-------------|
| 0x80-0xBF | `10xxxxxx` | Small block: tag = (byte >> 4) & 0xF, size = byte & 0x7 |
| 0x40-0x7F | `01xxxxxx` | Small int: value = byte & 0x3F (0-63) |
| 0x20-0x3F | `001xxxxx` | Small string: length = byte & 0x1F (0-31) |

#### Explicit Codes
| Code | Value | Description |
|------|-------|-------------|
| `CODE_INT8` | 0x00 | 8-bit signed integer follows |
| `CODE_INT16` | 0x01 | 16-bit signed integer follows (big-endian) |
| `CODE_INT32` | 0x02 | 32-bit signed integer follows (big-endian) |
| `CODE_INT64` | 0x03 | 64-bit signed integer follows (big-endian) |
| `CODE_SHARED8` | 0x04 | Reference to shared object (8-bit distance) |
| `CODE_SHARED16` | 0x05 | Reference to shared object (16-bit distance) |
| `CODE_SHARED32` | 0x06 | Reference to shared object (32-bit distance) |
| `CODE_BLOCK32` | 0x08 | Block with 32-bit header (tag + size) |
| `CODE_STRING8` | 0x09 | String with 8-bit length |
| `CODE_STRING32` | 0x0A | String with 32-bit length |
| `CODE_DOUBLE_LITTLE` | 0x0C | 64-bit float, little-endian |
| `CODE_DOUBLE_BIG` | 0x0B | 64-bit float, big-endian |

### Value Encoding

#### Integers
```rust
fn write_int(w: &mut Writer, n: i64) {
    if n >= 0 && n < 0x40 {
        w.write_u8(PREFIX_SMALL_INT + (n as u8));  // Single byte
    } else if n >= -128 && n < 128 {
        w.write_u8(CODE_INT8);
        w.write_i8(n as i8);
    } else if n >= -32768 && n < 32768 {
        w.write_u8(CODE_INT16);
        w.write_i16_be(n as i16);
    } else if n >= -1073741824 && n < 1073741824 {
        w.write_u8(CODE_INT32);
        w.write_i32_be(n as i32);
    } else {
        w.write_u8(CODE_INT64);
        w.write_i64_be(n);
    }
}
```

#### Strings
```rust
fn write_string(w: &mut Writer, s: &[u8]) {
    let len = s.len();
    if len < 0x20 {
        w.write_u8(PREFIX_SMALL_STRING + (len as u8));
    } else if len < 0x100 {
        w.write_u8(CODE_STRING8);
        w.write_u8(len as u8);
    } else {
        w.write_u8(CODE_STRING32);
        w.write_u32_be(len as u32);
    }
    w.write_bytes(s);
}
```

#### Blocks (Records, Variants, Tuples)

**CRITICAL**: Small block encoding is `PREFIX_SMALL_BLOCK + tag + (sz << 4)`:
- Bits 0-3: tag (0-15)
- Bits 4-6: size (0-7)
- Bit 7: always 1 (PREFIX_SMALL_BLOCK = 0x80)

```rust
fn write_block_header(w: &mut Writer, tag: u8, size: usize) {
    if tag < 16 && size < 8 {
        w.write_u8(PREFIX_SMALL_BLOCK + tag + ((size as u8) << 4));
    } else {
        let header = ((size as u64) << 10) | (tag as u64);
        w.write_u8(CODE_BLOCK32);
        w.write_u32_be(header as u32);
    }
}
```

---

## OCaml's Object Model and Sharing Semantics

### Memory Model

OCaml values are either:
1. **Immediate values** (integers, booleans, unit) - encoded directly in 63 bits
2. **Heap-allocated blocks** (records, tuples, variants with data, strings) - referenced by pointers

```ocaml
(* Same pointer - physically identical *)
let pos1 = {pos_fname = "test.res"; pos_lnum = 1; pos_bol = 0; pos_cnum = 5}
let pos2 = pos1  (* pos2 points to SAME memory object *)
(* pos1 == pos2 is TRUE *)

(* Different pointers - two allocations *)
let pos1 = {pos_fname = "test.res"; pos_lnum = 1; pos_bol = 0; pos_cnum = 5}
let pos2 = {pos_fname = "test.res"; pos_lnum = 1; pos_bol = 0; pos_cnum = 5}
(* pos1 == pos2 is FALSE (different pointers) *)
(* pos1 = pos2 is TRUE (same content) *)
```

### Marshal's Sharing Detection

OCaml's Marshal (runtime/extern.c) maintains a hash table mapping **heap addresses** to object indices:

```c
// Pseudocode from OCaml runtime
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

### Shared Reference Distance

The distance is `current_obj_counter - position_when_first_written`:

| Distance | Code | Format |
|----------|------|--------|
| 1-255 | CODE_SHARED8 (0x04) | 1 byte offset |
| 256-65535 | CODE_SHARED16 (0x05) | 2 bytes big-endian |
| 65536+ | CODE_SHARED32 (0x06) | 4 bytes big-endian |

### Object Counter Semantics

**Critical rule:** The object counter is incremented AFTER writing each heap object.

```
Initial: obj_counter = 0

Write block (tag=0, size=2):
  1. Write header byte
  2. obj_counter is STILL 0 (this object gets index 0)
  3. Write fields
  4. Increment: obj_counter = 1

Write shared reference to object 0:
  1. Distance = obj_counter - 0 = 1
  2. Write CODE_SHARED8, 0x01
  3. obj_counter stays 1 (shared refs don't increment!)
```

---

## Position Allocation in the ReScript Parser

### Scanner Position Creation

In `compiler/syntax/src/res_scanner.ml`:

```ocaml
let position scanner =
  Lexing.{
    pos_fname = scanner.filename;
    pos_lnum = scanner.lnum;
    pos_bol = scanner.line_offset;
    pos_cnum = scanner.line_offset + scanner.offset16;
  }
```

**Each call allocates a NEW record** on the heap with a unique pointer address.

### Parser Position Assignment

In `compiler/syntax/src/res_parser.ml`:

```ocaml
let rec next ?prev_end_pos p =
  let prev_end_pos =
    match prev_end_pos with
    | Some pos -> pos           (* Use explicitly provided position *)
    | None -> p.end_pos         (* REUSE existing end_pos object! *)
  in
  let start_pos, end_pos, token = Scanner.scan p.scanner in
  (* start_pos and end_pos are NEW objects from scanner *)

  p.prev_end_pos <- prev_end_pos;  (* SAME object as previous end_pos *)
  p.start_pos <- start_pos;        (* NEW object from scanner *)
  p.end_pos <- end_pos             (* NEW object from scanner *)
```

### The Critical Sharing Pattern

When parsing `let x = 1`:

```
Token 1: "let" at positions 0-3
  Scanner creates: start_pos_A (pointer P1), end_pos_A (pointer P2)
  Parser assigns: p.start_pos ← P1, p.end_pos ← P2

Token 2: "x" at positions 4-5
  Scanner creates: start_pos_B (pointer P3), end_pos_B (pointer P4)
  Parser assigns:
    prev_end_pos ← p.end_pos (which is P2!)  ← SHARING HERE
    p.start_pos ← P3
    p.end_pos ← P4
    p.prev_end_pos ← prev_end_pos (P2)
```

**Result:** Position P2 is now referenced by BOTH token 1's end and token 2's prev_end_pos. Marshal detects this shared pointer and writes CODE_SHARED*.

### Position Sharing Summary

| Source | Creates New Object? | Shared With |
|--------|---------------------|-------------|
| Scanner.scan start_pos | YES (new pointer) | Nothing (unique) |
| Scanner.scan end_pos | YES (new pointer) | Next token's prev_end_pos |
| prev_end_pos assignment | NO (reuses pointer) | Previous token's end_pos |
| Comment.set_prev_tok_end_pos | NO (stores reference) | Current token's end_pos |
| Lexing.dummy_pos | NO (global constant) | All initial parser positions |

---

## Current Rust Implementation Analysis

### PositionId System

The Rust parser uses `PositionId` to track object identity:

```rust
pub struct PositionId(u32);

pub struct Position {
    pub file_name: String,
    pub line: i32,
    pub bol: i32,
    pub cnum: i32,
    pub id: PositionId,  // Identity marker
}
```

When a Position is **cloned**, the `id` is **copied** (same identity).

### Scanner Position Creation

```rust
pub fn position(&mut self) -> Position {
    let id = PositionId::from_raw(self.position_id_counter);
    self.position_id_counter += 1;  // NEW ID each time
    Position::new_with_id(&self.filename, self.lnum, ...)
}
```

Each call creates a position with a **unique ID**. This matches OCaml.

### Parser Position Assignment

```rust
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

**Good:** The clone preserves the PositionId.

### The Problem: Content-Based Sharing in Marshal

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

**Problem:** The `PositionId` is completely ignored. Sharing is content-based, not identity-based.

### Why Content-Based Sharing Differs

**Scenario 1: Two positions with same content but different identities**

```ocaml
(* OCaml: Different pointers → NOT shared *)
let pos1 = position scanner  (* NEW object P1 *)
let pos2 = position scanner  (* NEW object P2, same content *)
(* Marshal: NOT shared *)
```

```rust
// Rust with content-based: SHARED (wrong!)
let pos1 = scanner.position();  // id=42, content=(1, 0, 5)
let pos2 = scanner.position();  // id=43, content=(1, 0, 5)
// Content match → Marshal: SHARED (wrong!)
```

**Scenario 2: Same position assigned to multiple fields**

```ocaml
(* OCaml: Same pointer → SHARED *)
let pos = position scanner  (* P1 *)
loc1.start = pos  (* References P1 *)
loc2.start = pos  (* References P1 *)
(* Marshal: SHARED (correct) *)
```

```rust
// Rust: Works correctly (same ID via clone)
let pos = scanner.position();  // id=42
let loc1_start = pos.clone();  // id=42
let loc2_start = pos.clone();  // id=42
// Same ID → Marshal: SHARED (correct)
```

---

## The Solution

### 1. Switch to Identity-Based Position Sharing

Modify `marshal.rs` to use `PositionId`:

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
        return false;
    }

    let obj_idx = self.obj_counter;

    self.write_block_header(0, 4);
    self.write_string_shared(file_name);
    self.write_int(line as i64);
    self.write_int(bol as i64);
    self.write_int(cnum as i64);

    self.position_table.insert(id, obj_idx);
    true
}
```

### 2. Ensure Correct PositionId Assignment

| OCaml Pattern | Rust Equivalent | PositionId Behavior |
|---------------|-----------------|---------------------|
| `pos = Scanner.position()` | `pos = scanner.position()` | New unique ID |
| `pos2 = pos` (assignment) | `pos2 = pos.clone()` | Same ID (shared) |
| `field <- pos` (mutable field) | `self.field = pos` | Move, ID preserved |

### 3. Handle Backtracking

Scanner backtracking currently rewinds the position_id_counter:

```rust
pub fn restore(&mut self, snapshot: ScannerSnapshot) {
    self.position_id_counter = snapshot.position_id_counter;  // REWINDS!
}
```

This may be acceptable because backtracked positions are discarded. Verify through testing.

### 4. LocationId Sharing

Location sharing should also use identity:

```rust
impl Marshal for Location {
    fn marshal(&self, w: &mut MarshalWriter) {
        if let Some(obj_idx) = w.get_location_by_id(self.id) {
            let d = w.obj_counter() - obj_idx;
            w.write_shared_ref(d);
            return;
        }

        let obj_idx = w.obj_counter();
        w.write_block_header(0, 3);
        self.loc_start.marshal(w);
        self.loc_end.marshal(w);
        w.write_int(if self.loc_ghost { 1 } else { 0 });

        w.record_location_id(self.id, obj_idx);
    }
}
```

### 5. String Interning

OCaml's parser interns certain strings via `Token.to_string`:

```ocaml
let to_string = function
  | True -> "true"      (* Same pointer every time *)
  | False -> "false"
  | Plus -> "+"
  (* ... *)
```

Match in Rust:
```rust
fn is_interned_string(s: &str) -> bool {
    matches!(s, "true" | "false" | "()" | "[]" | "==" | "!=" |
               "+" | "-" | "*" | "/" | ...)
}

pub fn write_identifier_string(&mut self, s: &str) {
    if Self::is_interned_string(s) {
        self.write_string_shared(s);  // Content-based (correct for strings)
    } else {
        self.write_str(s);
    }
}
```

---

## Type System Mapping

### Core Types

#### Lexing.position
```ocaml
type position = {
  pos_fname : string;
  pos_lnum : int;
  pos_bol : int;
  pos_cnum : int;
}
```

#### Location.t
```ocaml
type t = {
  loc_start: Lexing.position;
  loc_end: Lexing.position;
  loc_ghost: bool;
}
```

#### Location.loc
```ocaml
type 'a loc = { txt: 'a; loc: t }
```

#### Longident.t
```ocaml
type t =
  | Lident of string      (* tag 0 *)
  | Ldot of t * string    (* tag 1 *)
  | Lapply of t * t       (* tag 2 *)
```

### Asttypes (all constant constructors = integers)

| Type | Constructor | Value |
|------|-------------|-------|
| rec_flag | Nonrecursive | 0 |
| rec_flag | Recursive | 1 |
| direction_flag | Upto | 0 |
| direction_flag | Downto | 1 |
| private_flag | Private | 0 |
| private_flag | Public | 1 |
| mutable_flag | Immutable | 0 |
| mutable_flag | Mutable | 1 |
| closed_flag | Closed | 0 |
| closed_flag | Open | 1 |

### Noloc.arg_label (mixed)

| Constructor | Encoding |
|-------------|----------|
| Nolabel | int 0 |
| Labelled(s) | Block(tag=0, [s]) |
| Optional(s) | Block(tag=1, [s]) |

### Parsetree0 → Current Parsetree Transformations

The mapper in `ast_mapper_to0.ml` handles:

| Current | Parsetree0 |
|---------|------------|
| `Pexp_await(expr)` | Add `[@res.await]` attribute |
| `Pexp_apply { partial: true }` | Add `[@res.partial]` attribute |
| JSX nodes | Function application with `[@JSX]` |
| `->` operator | `\|.` (pipe) |
| `++` | `^` (string concat) |
| `==` | `=` (structural equality) |
| `===` | `==` (physical equality) |
| `!=` | `<>` (structural inequality) |
| `!==` | `!=` (physical inequality) |

---

## Implementation Phases

### Phase 1: Identity-Based Position Sharing
- [ ] Modify `write_position_shared` to use `PositionId` instead of content
- [ ] Remove or deprecate `position_content_table`
- [ ] Update all callers to pass actual PositionId
- [ ] Add unit tests for identity-based sharing

### Phase 2: Verify Parser Position Assignment
- [ ] Audit all places where positions are assigned/cloned
- [ ] Ensure `prev_end_pos = end_pos.clone()` pattern is consistent
- [ ] Verify comments store position references correctly
- [ ] Add debug logging to trace position ID flow

### Phase 3: Location Sharing
- [ ] Verify LocationId-based sharing works correctly
- [ ] Remove redundant sharing tables
- [ ] Add unit tests for location sharing

### Phase 4: Integration Testing
- [ ] Create binary comparison test: `diff rust.ast ocaml.ast`
- [ ] Test on all syntax test files
- [ ] Analyze remaining differences with hex diff
- [ ] Create minimal reproduction cases for failures

### Phase 5: Edge Cases
- [ ] Test backtracking scenarios (lookahead in parser)
- [ ] Test comment handling
- [ ] Test very large files (many positions)
- [ ] Test files with repeated identical constructs

---

## Testing Strategy

### Level 1: Unit Tests
```rust
#[test]
fn test_marshal_small_int() {
    let mut w = MarshalWriter::new();
    w.write_int(42);
    assert_eq!(w.buffer, vec![0x6A]); // PREFIX_SMALL_INT | 42
}
```

### Level 2: Type Serialization Tests
```rust
#[test]
fn test_location_serialization() {
    let loc = Location { ... };
    let mut w = MarshalWriter::new();
    loc.marshal(&mut w);
    let rust_output = w.finish();
    let ocaml_output = include_bytes!("testdata/location_marshal.bin");
    assert_eq!(rust_output, ocaml_output);
}
```

### Level 3: Binary Comparison Tests
```bash
#!/bin/bash
for file in tests/syntax_tests/data/parsing/grammar/**/*.res; do
    ./ocaml_parser -print binary "$file" > /tmp/ocaml.ast
    ./rust_parser --print binary "$file" > /tmp/rust.ast
    if ! diff /tmp/ocaml.ast /tmp/rust.ast; then
        echo "MISMATCH: $file"
        exit 1
    fi
done
echo "All files match!"
```

### Level 4: PPX Compatibility Tests
```bash
# Generate AST with Rust
./rust_compiler -bs-ast test.res -o test.ast

# Run PPX on it
./ppx_react test.ast -o test_transformed.ast

# Verify transformed AST is valid
./ocaml_compiler -bs-read-ast test_transformed.ast
```

---

## Debugging Tools

### Hex Diff Analysis
```bash
./ocaml_parser -print binary test.res > /tmp/ocaml.ast
./rust_parser --print binary test.res > /tmp/rust.ast
xxd /tmp/ocaml.ast > /tmp/ocaml.hex
xxd /tmp/rust.ast > /tmp/rust.hex
diff /tmp/ocaml.hex /tmp/rust.hex | head -100
```

### Marshal Debug Tool
Use `scripts/debug_marshal.ml` to decode Marshal structure.

### Position ID Tracing
```rust
// In scanner.rs
pub fn position(&mut self) -> Position {
    let id = self.position_id_counter;
    eprintln!("Scanner: new position id={} at line={}", id, self.lnum);
    // ...
}
```

### Sharing Pattern Analysis
```rust
// In marshal.rs
if let Some(&obj_idx) = self.position_table.get(&id) {
    eprintln!("SHARE position id={} (obj {} -> {})", id.raw(), obj_idx, self.obj_counter);
}
```

---

## Risk Analysis

### High Risk

#### Sharing Semantics Mismatch
**Risk:** Rust position IDs don't match OCaml's pointer sharing exactly.
**Mitigation:** Comprehensive testing, debug tracing, incremental changes.

#### Variant Tag Assignment
**Risk:** Getting block tags wrong breaks everything.
**Mitigation:** Document all tag assignments, automated verification.

### Medium Risk

#### Backtracking Changes Sharing Pattern
**Risk:** Scanner backtracking reuses IDs unexpectedly.
**Mitigation:** Test backtracking scenarios, may need to not rewind counter.

#### String Interning Differences
**Risk:** Different set of strings interned.
**Mitigation:** Analyze OCaml's Token.to_string, match exactly.

### Low Risk

#### Position Value Calculation
**Risk:** UTF-16 calculation bugs affect position values.
**Mitigation:** Already passing sexp parity (values are correct).

---

## Success Criteria

1. **Binary size match:** Identical byte counts
2. **Object count match:** Same obj_count in header
3. **Byte-for-byte identical:** `diff rust.ast ocaml.ast` empty
4. **All syntax tests pass:** 100% of test files match
5. **No sexp regression:** Sexp parity remains 100%

---

## References

### OCaml Sources
- `runtime/extern.c` - Marshal serialization
- `runtime/caml/intext.h` - Marshal format constants
- `runtime/intern.c` - Marshal deserialization

### ReScript OCaml Sources
- `compiler/syntax/src/res_scanner.ml` - Position allocation
- `compiler/syntax/src/res_parser.ml` - Position assignment
- `compiler/depends/binary_ast.ml` - Binary AST writing
- `compiler/ml/parsetree0.ml` - Frozen PPX-compatible AST
- `compiler/common/ast_mapper_to0.ml` - Current → parsetree0 mapping

### Rust Implementation
- `compiler-rust/src/location.rs` - Position/Location types
- `compiler-rust/src/parser/scanner.rs` - Position creation
- `compiler-rust/src/parser/state.rs` - Parser position tracking
- `compiler-rust/src/binary_ast/marshal.rs` - Marshal serialization

---

## Appendices

### Appendix A: Complete Tag Assignment Reference

#### constant (all non-constant)
| Constructor | Tag |
|-------------|-----|
| Pconst_integer(s, char_opt) | 0 |
| Pconst_char(i) | 1 |
| Pconst_string(s, delim_opt) | 2 |
| Pconst_float(s, char_opt) | 3 |

#### Longident.t
| Constructor | Tag |
|-------------|-----|
| Lident(s) | 0 |
| Ldot(t, s) | 1 |
| Lapply(t1, t2) | 2 |

### Appendix B: MarshalWriter Structure

```rust
pub struct MarshalWriter {
    buffer: Vec<u8>,
    obj_counter: u32,
    size_32: u32,
    size_64: u32,

    // Identity-based sharing (primary for AST parity)
    position_table: HashMap<PositionId, u32>,
    location_id_table: HashMap<LocationId, u32>,

    // Content-based sharing (for strings)
    string_table: HashMap<String, u32>,

    // DEPRECATED after switching to identity-based:
    position_content_table: HashMap<(i32, i32, i32), u32>,
}
```

### Appendix C: Test Matrix

| Test Category | Example | Expected Behavior |
|---------------|---------|-------------------|
| Simple binding | `let x = 1` | prev_end_pos shared with prior end_pos |
| Multiple bindings | `let a = 1; let b = 2` | Each binding unique positions |
| Comments | `let x = 1 (* c *)` | Comment stores position reference |
| Backtracking | Complex patterns | IDs may be non-sequential |
| Large file | 1000+ tokens | Many shared references |

---

*Document created: 2026-01-20*
*Consolidated from: BINARY_AST_PLAN.md, BINARY_AST_PARITY_PLAN.md, BINARY_AST_PARITY_COMPREHENSIVE_PLAN.md*
