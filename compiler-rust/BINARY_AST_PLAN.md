# Binary AST Implementation Plan

This document outlines the plan for implementing byte-identical binary AST output in the Rust compiler, matching the OCaml implementation exactly.

## Table of Contents

1. [Overview](#overview)
2. [Goals and Constraints](#goals-and-constraints)
3. [Binary AST File Format](#binary-ast-file-format)
4. [OCaml Marshal Format Specification](#ocaml-marshal-format-specification)
5. [Type System Mapping](#type-system-mapping)
6. [Implementation Phases](#implementation-phases)
7. [Module Structure](#module-structure)
8. [Testing Strategy](#testing-strategy)
9. [Risk Analysis](#risk-analysis)
10. [Open Questions](#open-questions)
11. [References](#references)

---

## Overview

The ReScript compiler uses binary AST files (`.ast` for implementations, `.iast` for interfaces) as an intermediate format for:

1. **Incremental compilation**: Cached parsed ASTs avoid re-parsing unchanged files
2. **PPX support**: Preprocessor extensions read and write binary ASTs
3. **Build system integration**: `rewatch` uses binary ASTs for dependency tracking

Currently, only the OCaml compiler can generate these files. To complete the Rust compiler rewrite, we must generate byte-identical binary AST files without using OCaml FFI.

### Why Byte-Identical?

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
4. **Platform compatibility**: Must work on all supported platforms (darwin-arm64, darwin-x64, linux-x64, win32-x64)

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
3. A `/` character that:
   - Terminates the dependency list
   - Serves as the first character of the absolute source path

The source path follows, terminated by `\n`.

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

#### Section 3: OCaml Marshal Data

See [OCaml Marshal Format Specification](#ocaml-marshal-format-specification) below.

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
  output_value oc pt;  (* <-- OCaml Marshal *)
  close_out oc
```

---

## OCaml Marshal Format Specification

The OCaml Marshal module serializes arbitrary OCaml values to a binary format. This section documents the format in detail.

### Magic Numbers

```rust
const MAGIC_NUMBER_SMALL: u32 = 0x8495A6BE;  // Standard format
const MAGIC_NUMBER_BIG: u32 = 0x8495A6BF;    // Large objects (>4GB)
const MAGIC_NUMBER_COMPRESSED: u32 = 0x8495A6BD;  // Compressed format
```

We only need `MAGIC_NUMBER_SMALL` for AST files.

### Header Format (20 bytes for small format)

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

These use the high bits to encode type information:

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
| `CODE_SHARED8` | 0x04 | Reference to shared object (8-bit index) |
| `CODE_SHARED16` | 0x05 | Reference to shared object (16-bit index) |
| `CODE_SHARED32` | 0x06 | Reference to shared object (32-bit index) |
| `CODE_BLOCK32` | 0x08 | Block with 32-bit header (tag + size) |
| `CODE_STRING8` | 0x09 | String with 8-bit length |
| `CODE_STRING32` | 0x0A | String with 32-bit length |
| `CODE_DOUBLE_BIG` | 0x0B | 64-bit float, big-endian |
| `CODE_DOUBLE_LITTLE` | 0x0C | 64-bit float, little-endian |
| `CODE_DOUBLE_ARRAY8_BIG` | 0x0D | Float array, 8-bit count, big-endian |
| `CODE_DOUBLE_ARRAY8_LITTLE` | 0x0E | Float array, 8-bit count, little-endian |
| `CODE_DOUBLE_ARRAY32_BIG` | 0x0F | Float array, 32-bit count, big-endian |
| `CODE_DOUBLE_ARRAY32_LITTLE` | 0x07 | Float array, 32-bit count, little-endian |
| `CODE_CODEPOINTER` | 0x10 | Code pointer (closures) |
| `CODE_INFIXPOINTER` | 0x11 | Infix pointer (closures) |
| `CODE_BLOCK64` | 0x13 | Block with 64-bit header |
| `CODE_SHARED64` | 0x14 | Reference to shared object (64-bit index) |
| `CODE_STRING64` | 0x15 | String with 64-bit length |
| `CODE_DOUBLE_ARRAY64_BIG` | 0x16 | Float array, 64-bit count, big-endian |
| `CODE_DOUBLE_ARRAY64_LITTLE` | 0x17 | Float array, 64-bit count, little-endian |
| `CODE_CUSTOM_LEN` | 0x18 | Custom block with length |
| `CODE_CUSTOM_FIXED` | 0x19 | Custom block, fixed size |

### Byte Order

**All multi-byte integers use BIG-ENDIAN (network byte order)**

From OCaml source (`runtime/extern.c`):

```c
Caml_inline void store16(char * dst, int n) {
  dst[0] = n >> 8;  dst[1] = n;
}

Caml_inline void store32(char * dst, intnat n) {
  dst[0] = n >> 24;  dst[1] = n >> 16;  dst[2] = n >> 8;  dst[3] = n;
}

Caml_inline void store64(char * dst, int64_t n) {
  dst[0] = n >> 56;  dst[1] = n >> 48;  dst[2] = n >> 40;  dst[3] = n >> 32;
  dst[4] = n >> 24;  dst[5] = n >> 16;  dst[6] = n >> 8;   dst[7] = n;
}
```

### Value Encoding

#### Integers

From OCaml source (`runtime/extern.c`):

```c
Caml_inline void extern_int(struct caml_extern_state* s, intnat n)
{
  if (n >= 0 && n < 0x40) {
    writebyte(s, PREFIX_SMALL_INT + n);
  } else if (n >= -(1 << 7) && n < (1 << 7)) {
    writecode8(s, CODE_INT8, n);
  } else if (n >= -(1 << 15) && n < (1 << 15)) {
    writecode16(s, CODE_INT16, n);
  } else if (n < -((intnat)1 << 30) || n >= ((intnat)1 << 30)) {
    writecode64(s, CODE_INT64, n);  // 64-bit only
  } else {
    writecode32(s, CODE_INT32, n);
  }
}
```

Rust implementation:

```rust
fn write_int(w: &mut Writer, n: i64) {
    if n >= 0 && n < 0x40 {
        // Small int: single byte 0x40 | n
        w.write_u8(PREFIX_SMALL_INT + (n as u8));
    } else if n >= -128 && n < 128 {
        w.write_u8(CODE_INT8);
        w.write_i8(n as i8);
    } else if n >= -32768 && n < 32768 {
        w.write_u8(CODE_INT16);
        w.write_i16_be(n as i16);
    } else if n >= -1073741824 && n < 1073741824 {  // -(1<<30) to (1<<30)
        w.write_u8(CODE_INT32);
        w.write_i32_be(n as i32);
    } else {
        w.write_u8(CODE_INT64);
        w.write_i64_be(n);
    }
}
```

#### Strings

From OCaml source (`runtime/extern.c`):

```c
Caml_inline void extern_string(struct caml_extern_state *s,
                               value v, mlsize_t len)
{
  if (len < 0x20) {
    writebyte(s, PREFIX_SMALL_STRING + len);
  } else if (len < 0x100) {
    writecode8(s, CODE_STRING8, len);
  } else {
    if (len < (uintnat)1 << 32)
      writecode32(s, CODE_STRING32, len);
    else
      writecode64(s, CODE_STRING64, len);
  }
  writeblock(s, String_val(v), len);
}
```

Rust implementation:

```rust
fn write_string(w: &mut Writer, s: &[u8]) {
    let len = s.len();
    if len < 0x20 {
        // Small string: PREFIX_SMALL_STRING + len
        w.write_u8(PREFIX_SMALL_STRING + (len as u8));
    } else if len < 0x100 {
        w.write_u8(CODE_STRING8);
        w.write_u8(len as u8);
    } else if len < 0x100000000 {
        w.write_u8(CODE_STRING32);
        w.write_u32_be(len as u32);
    } else {
        w.write_u8(CODE_STRING64);
        w.write_u64_be(len as u64);
    }
    w.write_bytes(s);
}
```

**Note**: Use `&[u8]` not `&str` to handle potential non-UTF8 strings in OCaml.

#### Blocks (Records, Variants, Tuples)

From OCaml source (`runtime/extern.c`):

```c
Caml_inline void extern_header(struct caml_extern_state* s,
                               mlsize_t sz, tag_t tag)
{
  if (tag < 16 && sz < 8) {
    writebyte(s, PREFIX_SMALL_BLOCK + tag + (sz << 4));
  } else {
    header_t hd = Make_header(sz, tag, NOT_MARKABLE);
    if (hd < (uintnat)1 << 32)
      writecode32(s, CODE_BLOCK32, hd);
    else
      writecode64(s, CODE_BLOCK64, hd);
  }
}
```

**CRITICAL**: The small block encoding is `PREFIX_SMALL_BLOCK + tag + (sz << 4)`, NOT `(tag << 4) | sz`!

- Bits 0-3: tag (0-15)
- Bits 4-6: size (0-7)
- Bit 7: always 1 (from PREFIX_SMALL_BLOCK = 0x80)

For larger blocks, OCaml's `Make_header(sz, tag, color)` creates:
- On 32-bit: `(sz << 10) | (color << 8) | tag`
- On 64-bit: `(sz << 10) | (color << 8) | tag`

Since `NOT_MARKABLE` color is typically 0, the header is: `(sz << 10) | tag`

Rust implementation:

```rust
fn write_block_header(w: &mut Writer, tag: u8, size: usize) {
    if tag < 16 && size < 8 {
        // Small block: PREFIX_SMALL_BLOCK + tag + (size << 4)
        w.write_u8(PREFIX_SMALL_BLOCK + tag + ((size as u8) << 4));
    } else {
        // Large block: header = (size << 10) | tag
        let header = ((size as u64) << 10) | (tag as u64);
        if header < (1u64 << 32) {
            w.write_u8(CODE_BLOCK32);
            w.write_u32_be(header as u32);
        } else {
            w.write_u8(CODE_BLOCK64);
            w.write_u64_be(header);
        }
    }
}
```

#### Lists

OCaml lists are linked cons cells:

```ocaml
type 'a list = [] | (::) of 'a * 'a list
```

Encoding:
- `[]` → integer 0
- `[a; b; c]` → `Block(tag=0, [a, Block(tag=0, [b, Block(tag=0, [c, 0])])])`

```rust
fn write_list<T: Marshal>(w: &mut Writer, items: &[T]) {
    if items.is_empty() {
        w.write_int(0);  // Empty list = int 0
    } else {
        // Build from end
        let mut rest = vec![];
        for item in items.iter().rev() {
            // Cons cell: tag 0, size 2
            w.write_block(0, 2);
            item.marshal(w);
            if rest.is_empty() {
                w.write_int(0);  // Nil
            } else {
                // Write the accumulated rest
                w.write_bytes(&rest);
            }
            // This approach doesn't work - need different strategy
        }
    }
}
```

**Note**: Lists require careful handling because they're built recursively. Two approaches:

1. **Recursive serialization** (natural but may stack overflow for long lists)
2. **Pre-compute structure** then serialize (more complex but safer)

#### Options

```ocaml
type 'a option = None | Some of 'a
```

Encoding:
- `None` → integer 0 (constant constructor, index 0)
- `Some(v)` → `Block(tag=0, [v])` (non-constant constructor, tag 0)

```rust
fn write_option<T: Marshal>(w: &mut Writer, opt: &Option<T>) {
    match opt {
        None => w.write_int(0),
        Some(v) => {
            w.write_block(0, 1);
            v.marshal(w);
        }
    }
}
```

#### Booleans

```ocaml
type bool = false | true
```

Encoding:
- `false` → integer 0
- `true` → integer 1

#### Unit

```ocaml
type unit = ()
```

Encoding: integer 0

#### Tuples

Tuples are blocks with tag 0:

```ocaml
(a, b, c)  (* 3-tuple *)
```

Encoding: `Block(tag=0, [a, b, c])`

#### Records

Records are blocks with tag 0, fields in declaration order:

```ocaml
type t = { x: int; y: string; z: bool }
{ x = 1; y = "hi"; z = true }
```

Encoding: `Block(tag=0, [1, "hi", 1])`

#### Variants

Variants use a combination of integers and blocks:

```ocaml
type t = A | B | C of int | D of string * bool
```

- Constant constructors (no data): integers starting from 0
  - `A` → integer 0
  - `B` → integer 1
- Non-constant constructors (with data): blocks with tags starting from 0
  - `C(42)` → `Block(tag=0, [42])`
  - `D("x", true)` → `Block(tag=1, ["x", 1])`

**Important**: Tags are assigned based on the order of non-constant constructors, not the order in the type definition.

### Sharing (Object References)

When the same object appears multiple times, Marshal serializes it once and uses `CODE_SHARED*` for subsequent occurrences.

From OCaml source (`runtime/extern.c`):

```c
// Lookup: returns 1 if found, 0 if not found
Caml_inline int extern_lookup_position(struct caml_extern_state *s, value obj,
                                       uintnat * pos_out, uintnat * h_out)
{
  uintnat h = Hash(obj, s->pos_table.shift);
  while (1) {
    if (! bitvect_test(s->pos_table.present, h)) {
      *h_out = h;
      return 0;  // Not found
    }
    if (s->pos_table.entries[h].obj == obj) {
      *pos_out = s->pos_table.entries[h].pos;
      return 1;  // Found - return stored position
    }
    h = (h + 1) & s->pos_table.mask;  // Linear probing
  }
}

// Record location after serializing a new object
static void extern_record_location(struct caml_extern_state* s,
                                   value obj, uintnat h)
{
  if (s->extern_flags & NO_SHARING) return;
  bitvect_set(s->pos_table.present, h);
  s->pos_table.entries[h].obj = obj;
  s->pos_table.entries[h].pos = s->obj_counter;
  s->obj_counter++;
  if (s->obj_counter >= s->pos_table.threshold)
    extern_resize_position_table(s);
}

// Write shared reference
Caml_inline void extern_shared_reference(struct caml_extern_state* s,
                                         uintnat d)
{
  if (d < 0x100) {
    writecode8(s, CODE_SHARED8, d);
  } else if (d < 0x10000) {
    writecode16(s, CODE_SHARED16, d);
  } else if (d >= (uintnat)1 << 32) {
    writecode64(s, CODE_SHARED64, d);
  } else {
    writecode32(s, CODE_SHARED32, d);
  }
}
```

**Key insight**: The sharing reference `d` is calculated as `obj_counter - pos` (distance from current position), NOT the absolute position. This means:
- First object gets pos=0
- Second object gets pos=1
- If second object references first, d = 1 - 0 = 1

In the main serialization loop:
```c
if (extern_lookup_position(s, v, &pos, &h)) {
  uintnat d = s->obj_counter - pos;  // Distance, not absolute position
  extern_shared_reference(s, d);
  goto next_item;
}
```

Rust implementation:

```rust
struct MarshalWriter {
    buffer: Vec<u8>,
    // Maps pointer identity to object counter at time of serialization
    obj_table: HashMap<usize, u32>,
    obj_counter: u32,
}

impl MarshalWriter {
    fn check_shared(&mut self, ptr: usize) -> bool {
        if let Some(&pos) = self.obj_table.get(&ptr) {
            let d = self.obj_counter - pos;  // Distance!
            self.write_shared_ref(d);
            true
        } else {
            false
        }
    }

    fn record_location(&mut self, ptr: usize) {
        self.obj_table.insert(ptr, self.obj_counter);
        self.obj_counter += 1;
    }

    fn write_shared_ref(&mut self, d: u32) {
        if d < 0x100 {
            self.write_u8(CODE_SHARED8);
            self.write_u8(d as u8);
        } else if d < 0x10000 {
            self.write_u8(CODE_SHARED16);
            self.write_u16_be(d as u16);
        } else {
            self.write_u8(CODE_SHARED32);
            self.write_u32_be(d);
        }
    }
}
```

**Challenge**: Achieving byte-identical output requires knowing exactly which objects OCaml decides to share. This depends on:
1. OCaml's memory allocation patterns (pointer identity)
2. Whether objects are physically equal (`==`) vs structurally equal (`=`)

**Mitigation strategies**:
1. Start with `No_sharing` mode (if OCaml uses it)
2. Analyze OCaml's sharing decisions empirically
3. Build AST with explicit sharing where OCaml would share

### Block Traversal Order (Depth-First)

From OCaml source - the critical traversal logic for multi-field blocks:

```c
// In extern_rec, for a regular block (not String, Double, etc):
default: {
  extern_header(s, sz, tag);
  s->size_32 += 1 + sz;
  s->size_64 += 1 + sz;
  extern_record_location(s, v, h);

  // Push remaining fields (1 to sz-1) onto stack
  if (sz > 1) {
    sp++;
    sp->v = &Field(v, 1);      // Pointer to field 1
    sp->count = sz - 1;        // Number of remaining fields
  }

  // Continue immediately with field 0
  v = Field(v, 0);
  continue;
}

// After processing all nested values, pop from stack:
next_item:
  if (sp == s->extern_stack) {
    // Stack empty - done
    return;
  }
  v = *((sp->v)++);           // Get next field, advance pointer
  if (--(sp->count) == 0) sp--; // Pop if no more fields
```

**This means**: For a block `{a; b; c}`:
1. Write block header (tag=0, size=3)
2. Record location for sharing
3. Push `&Field(v,1)` with count=2 onto stack
4. Process field 0 (`a`) recursively
5. Pop stack: process field 1 (`b`)
6. Pop stack: process field 2 (`c`)

The fields are serialized in order 0, 1, 2, ... using a depth-first traversal.

### Complete Serialization Example

For a nested structure `{x: {a: 1; b: 2}; y: 3}`:

```
1. Block header (tag=0, size=2)     <- outer record
2. Record location (obj_counter=0)
3. Push (y: 3) onto stack
4. Block header (tag=0, size=2)     <- inner record {a; b}
5. Record location (obj_counter=1)
6. Push (b: 2) onto stack
7. Int 1                            <- a
8. Pop: Int 2                       <- b
9. Pop: Int 3                       <- y
```

Output bytes (conceptual): `[block(0,2)] [block(0,2)] [int 1] [int 2] [int 3]`

### Strings Are NOT Recursively Traversed

**Important**: Strings are handled specially - they are NOT pushed onto the stack:

```c
case String_tag: {
  mlsize_t len = caml_string_length(v);
  extern_string(s, v, len);    // Write string directly
  s->size_32 += 1 + (len + 4) / 4;
  s->size_64 += 1 + (len + 8) / 8;
  extern_record_location(s, v, h);
  break;  // NOT continue - goes to next_item
}
```

---

## Type System Mapping

### Overview

The Rust parser produces a "current" parsetree. For binary AST output, we must convert to `parsetree0` (the frozen PPX-compatible version).

### Core Types

#### Lexing.position

```ocaml
(* OCaml Lexing.position *)
type position = {
  pos_fname : string;
  pos_lnum : int;
  pos_bol : int;
  pos_cnum : int;
}
```

```rust
// Rust equivalent
#[derive(Clone)]
pub struct Position {
    pub pos_fname: String,
    pub pos_lnum: i32,
    pub pos_bol: i32,
    pub pos_cnum: i32,
}

impl Marshal for Position {
    fn marshal(&self, w: &mut MarshalWriter) {
        w.write_block(0, 4);  // Record with 4 fields
        w.write_string(&self.pos_fname);
        w.write_int(self.pos_lnum as i64);
        w.write_int(self.pos_bol as i64);
        w.write_int(self.pos_cnum as i64);
    }
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

```rust
pub struct Location {
    pub loc_start: Position,
    pub loc_end: Position,
    pub loc_ghost: bool,
}

impl Marshal for Location {
    fn marshal(&self, w: &mut MarshalWriter) {
        w.write_block(0, 3);
        self.loc_start.marshal(w);
        self.loc_end.marshal(w);
        w.write_int(if self.loc_ghost { 1 } else { 0 });
    }
}
```

#### Location.loc (parameterized)

```ocaml
type 'a loc = { txt: 'a; loc: t }
```

```rust
pub struct Loc<T> {
    pub txt: T,
    pub loc: Location,
}

impl<T: Marshal> Marshal for Loc<T> {
    fn marshal(&self, w: &mut MarshalWriter) {
        w.write_block(0, 2);
        self.txt.marshal(w);
        self.loc.marshal(w);
    }
}
```

#### Longident.t

```ocaml
type t =
  | Lident of string      (* tag 0 *)
  | Ldot of t * string    (* tag 1 *)
  | Lapply of t * t       (* tag 2 *)
```

```rust
pub enum Longident {
    Lident(String),
    Ldot(Box<Longident>, String),
    Lapply(Box<Longident>, Box<Longident>),
}

impl Marshal for Longident {
    fn marshal(&self, w: &mut MarshalWriter) {
        match self {
            Longident::Lident(s) => {
                w.write_block(0, 1);
                w.write_string(s);
            }
            Longident::Ldot(lid, s) => {
                w.write_block(1, 2);
                lid.marshal(w);
                w.write_string(s);
            }
            Longident::Lapply(l1, l2) => {
                w.write_block(2, 2);
                l1.marshal(w);
                l2.marshal(w);
            }
        }
    }
}
```

### Asttypes

```ocaml
type rec_flag = Nonrecursive | Recursive
type direction_flag = Upto | Downto
type private_flag = Private | Public
type mutable_flag = Immutable | Mutable
type virtual_flag = Virtual | Concrete
type override_flag = Override | Fresh
type closed_flag = Closed | Open
type variance = Covariant | Contravariant | Invariant

(* All constant constructors - encode as integers *)
```

```rust
// All these are constant-only variants, so they encode as integers

pub enum RecFlag { Nonrecursive = 0, Recursive = 1 }
pub enum DirectionFlag { Upto = 0, Downto = 1 }
pub enum PrivateFlag { Private = 0, Public = 1 }
pub enum MutableFlag { Immutable = 0, Mutable = 1 }
pub enum VirtualFlag { Virtual = 0, Concrete = 1 }
pub enum OverrideFlag { Override = 0, Fresh = 1 }
pub enum ClosedFlag { Closed = 0, Open = 1 }
pub enum Variance { Covariant = 0, Contravariant = 1, Invariant = 2 }

impl Marshal for RecFlag {
    fn marshal(&self, w: &mut MarshalWriter) {
        w.write_int(*self as i64);
    }
}
// ... similar for others
```

#### Noloc.arg_label

```ocaml
type arg_label =
  | Nolabel                (* tag: int 0 *)
  | Labelled of string     (* tag: block 0 *)
  | Optional of string     (* tag: block 1 *)
```

```rust
pub enum ArgLabel {
    Nolabel,
    Labelled(String),
    Optional(String),
}

impl Marshal for ArgLabel {
    fn marshal(&self, w: &mut MarshalWriter) {
        match self {
            ArgLabel::Nolabel => w.write_int(0),
            ArgLabel::Labelled(s) => {
                w.write_block(0, 1);
                w.write_string(s);
            }
            ArgLabel::Optional(s) => {
                w.write_block(1, 1);
                w.write_string(s);
            }
        }
    }
}
```

### Parsetree0 Types

The full parsetree0 has approximately 50 types. Key complex types:

#### constant

```ocaml
type constant =
  | Pconst_integer of string * char option   (* block 0 *)
  | Pconst_char of int                       (* block 1 *)
  | Pconst_string of string * string option  (* block 2 *)
  | Pconst_float of string * char option     (* block 3 *)
```

#### core_type

```ocaml
type core_type = {
  ptyp_desc: core_type_desc;
  ptyp_loc: Location.t;
  ptyp_attributes: attributes;
}

and core_type_desc =
  | Ptyp_any                                        (* int 0 *)
  | Ptyp_var of string                              (* block 0 *)
  | Ptyp_arrow of arg_label * core_type * core_type (* block 1 *)
  | Ptyp_tuple of core_type list                    (* block 2 *)
  | Ptyp_constr of Longident.t loc * core_type list (* block 3 *)
  | Ptyp_object of object_field list * closed_flag  (* block 4 *)
  | Ptyp_class of unit                              (* block 5, dummy *)
  | Ptyp_alias of core_type * string                (* block 6 *)
  | Ptyp_variant of row_field list * closed_flag * label list option (* block 7 *)
  | Ptyp_poly of string loc list * core_type        (* block 8 *)
  | Ptyp_package of package_type                    (* block 9 *)
  | Ptyp_extension of extension                     (* block 10 *)
```

#### pattern

```ocaml
type pattern = {
  ppat_desc: pattern_desc;
  ppat_loc: Location.t;
  ppat_attributes: attributes;
}

and pattern_desc =
  | Ppat_any                                              (* int 0 *)
  | Ppat_var of string loc                                (* block 0 *)
  | Ppat_alias of pattern * string loc                    (* block 1 *)
  | Ppat_constant of constant                             (* block 2 *)
  | Ppat_interval of constant * constant                  (* block 3 *)
  | Ppat_tuple of pattern list                            (* block 4 *)
  | Ppat_construct of Longident.t loc * pattern option    (* block 5 *)
  | Ppat_variant of label * pattern option                (* block 6 *)
  | Ppat_record of (Longident.t loc * pattern) list * closed_flag (* block 7 *)
  | Ppat_array of pattern list                            (* block 8 *)
  | Ppat_or of pattern * pattern                          (* block 9 *)
  | Ppat_constraint of pattern * core_type                (* block 10 *)
  | Ppat_type of Longident.t loc                          (* block 11 *)
  | Ppat_lazy of pattern                                  (* block 12 *)
  | Ppat_unpack of string loc                             (* block 13 *)
  | Ppat_exception of pattern                             (* block 14 *)
  | Ppat_extension of extension                           (* block 15 *)
  | Ppat_open of Longident.t loc * pattern                (* block 16 *)
```

#### expression

```ocaml
type expression = {
  pexp_desc: expression_desc;
  pexp_loc: Location.t;
  mutable pexp_attributes: attributes;
}

and expression_desc =
  | Pexp_ident of Longident.t loc                         (* block 0 *)
  | Pexp_constant of constant                             (* block 1 *)
  | Pexp_let of rec_flag * value_binding list * expression (* block 2 *)
  | Pexp_function of case list                            (* block 3 *)
  | Pexp_fun of arg_label * expression option * pattern * expression (* block 4 *)
  | Pexp_apply of expression * (arg_label * expression) list (* block 5 *)
  | Pexp_match of expression * case list                  (* block 6 *)
  | Pexp_try of expression * case list                    (* block 7 *)
  | Pexp_tuple of expression list                         (* block 8 *)
  | Pexp_construct of Longident.t loc * expression option (* block 9 *)
  | Pexp_variant of label * expression option             (* block 10 *)
  | Pexp_record of (Longident.t loc * expression) list * expression option (* block 11 *)
  | Pexp_field of expression * Longident.t loc            (* block 12 *)
  | Pexp_setfield of expression * Longident.t loc * expression (* block 13 *)
  | Pexp_array of expression list                         (* block 14 *)
  | Pexp_ifthenelse of expression * expression * expression option (* block 15 *)
  | Pexp_sequence of expression * expression              (* block 16 *)
  | Pexp_while of expression * expression                 (* block 17 *)
  | Pexp_for of pattern * expression * expression * direction_flag * expression (* block 18 *)
  | Pexp_constraint of expression * core_type             (* block 19 *)
  | Pexp_coerce of expression * unit * core_type          (* block 20 *)
  | Pexp_send of expression * label loc                   (* block 21 *)
  | Pexp_new of Longident.t loc                           (* block 22 *)
  | Pexp_setinstvar of label loc * expression             (* block 23 *)
  | Pexp_override of (label loc * expression) list        (* block 24 *)
  | Pexp_letmodule of string loc * module_expr * expression (* block 25 *)
  | Pexp_letexception of extension_constructor * expression (* block 26 *)
  | Pexp_assert of expression                             (* block 27 *)
  | Pexp_lazy of expression                               (* block 28 *)
  | Pexp_poly of expression * core_type option            (* block 29 *)
  | Pexp_object of unit                                   (* block 30, dummy *)
  | Pexp_newtype of string loc * expression               (* block 31 *)
  | Pexp_pack of module_expr                              (* block 32 *)
  | Pexp_open of override_flag * Longident.t loc * expression (* block 33 *)
  | Pexp_extension of extension                           (* block 34 *)
  | Pexp_unreachable                                      (* int 0 *)
```

### Complete Type Count

| Category | Count |
|----------|-------|
| Core types (Location, Longident, etc.) | ~8 |
| Asttypes (flags, labels) | ~12 |
| Parsetree0 types | ~50 |
| **Total** | **~70** |

---

## Implementation Phases

### Phase 1: Marshal Writer Foundation

**Goal**: Implement the core Marshal binary format writer.

**Deliverables**:
- [ ] `MarshalWriter` struct with buffer management
- [ ] Primitive encoding: integers, strings, floats
- [ ] Block encoding with tag and size
- [ ] Header generation
- [ ] Unit tests for each encoding type

**Files**:
- `compiler-rust/src/binary_ast/marshal.rs`

**Estimated effort**: Medium

**Verification**:
- Write test values with Rust, read with OCaml's `Marshal.from_string`
- Compare raw bytes for simple values

### Phase 2: Basic Type Serialization

**Goal**: Implement Marshal trait for fundamental types.

**Deliverables**:
- [ ] `Marshal` trait definition
- [ ] Implementations for: `bool`, `i32`, `i64`, `String`, `Option<T>`, `Vec<T>`, tuples
- [ ] `Position`, `Location`, `Loc<T>` serialization
- [ ] `Longident` serialization
- [ ] Unit tests

**Files**:
- `compiler-rust/src/binary_ast/types.rs`
- `compiler-rust/src/binary_ast/serialize.rs`

**Estimated effort**: Medium

**Verification**:
- Create equivalent OCaml values, compare Marshal output

### Phase 3: Parsetree0 Types

**Goal**: Define all parsetree0 types in Rust.

**Deliverables**:
- [ ] All ~50 parsetree0 type definitions
- [ ] Correct tag assignments for all variants
- [ ] Marshal implementations for all types
- [ ] Unit tests for complex types

**Files**:
- `compiler-rust/src/binary_ast/parsetree0.rs`
- `compiler-rust/src/binary_ast/parsetree0_marshal.rs`

**Estimated effort**: High (most code volume)

**Verification**:
- Parse simple `.res` files with OCaml, serialize, compare bytes

### Phase 4: Parsetree Mapping

**Goal**: Convert current Rust parsetree to parsetree0.

**Deliverables**:
- [ ] Mapping functions for all types
- [ ] Handle any structural differences between versions
- [ ] Unit tests for mapping

**Files**:
- `compiler-rust/src/binary_ast/mapper_to0.rs`

**Estimated effort**: Medium

**Verification**:
- Round-trip: Rust parse → map to parsetree0 → serialize → OCaml deserialize → compare

### Phase 5: Dependency Extraction

**Goal**: Extract module dependencies from AST.

**Deliverables**:
- [ ] AST visitor for dependency collection
- [ ] Filter logic (exclude `*predef*`, empty strings)
- [ ] Sorted output for determinism
- [ ] Unit tests

**Files**:
- `compiler-rust/src/binary_ast/deps.rs`

**Estimated effort**: Low

**Verification**:
- Compare extracted deps with OCaml's `Ast_extract`

### Phase 6: Binary AST Writer

**Goal**: Complete binary AST file generation.

**Deliverables**:
- [ ] File format writer combining all sections
- [ ] Support for both `.ast` (structure) and `.iast` (signature)
- [ ] CLI integration in `bsc`
- [ ] Integration tests

**Files**:
- `compiler-rust/src/binary_ast/mod.rs`
- `compiler-rust/src/binary_ast/writer.rs`
- Updates to `compiler-rust/src/bin/bsc.rs`

**Estimated effort**: Low-Medium

**Verification**:
- Byte-for-byte comparison with OCaml output
- PPX compatibility tests

### Phase 7: Sharing Support (if needed)

**Goal**: Implement object sharing to match OCaml's behavior.

**Deliverables**:
- [ ] Object identity tracking
- [ ] Shared reference encoding
- [ ] Analysis of OCaml's sharing decisions

**Files**:
- Updates to `compiler-rust/src/binary_ast/marshal.rs`

**Estimated effort**: High (depends on complexity of OCaml's sharing)

**Verification**:
- Byte comparison on complex ASTs with shared substructures

---

## Module Structure

```
compiler-rust/src/binary_ast/
├── mod.rs              # Public API, re-exports
├── marshal.rs          # Core Marshal format writer
├── types.rs            # Fundamental type definitions
├── serialize.rs        # Marshal trait and basic impls
├── parsetree0.rs       # Parsetree0 type definitions
├── parsetree0_marshal.rs # Marshal impls for parsetree0
├── mapper_to0.rs       # Current parsetree → parsetree0
├── deps.rs             # Dependency extraction
└── writer.rs           # Binary AST file writer
```

### Public API

```rust
// compiler-rust/src/binary_ast/mod.rs

pub use writer::{write_structure_ast, write_signature_ast};

/// Write a binary AST file for a structure (implementation)
pub fn write_structure_ast(
    output_path: &Path,
    source_path: &str,
    ast: &crate::parser::Structure,
) -> io::Result<()>;

/// Write a binary AST file for a signature (interface)
pub fn write_signature_ast(
    output_path: &Path,
    source_path: &str,
    ast: &crate::parser::Signature,
) -> io::Result<()>;
```

---

## Testing Strategy

### Level 1: Unit Tests

Test individual components in isolation:

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_marshal_small_int() {
        let mut w = MarshalWriter::new();
        w.write_int(42);
        assert_eq!(w.buffer, vec![0x6A]); // PREFIX_SMALL_INT | 42
    }

    #[test]
    fn test_marshal_string() {
        let mut w = MarshalWriter::new();
        w.write_string("hello");
        // PREFIX_SMALL_STRING | 5, then "hello"
        assert_eq!(w.buffer, vec![0x25, b'h', b'e', b'l', b'l', b'o']);
    }
}
```

### Level 2: Type Serialization Tests

Test that Rust types serialize identically to OCaml:

```rust
#[test]
fn test_location_serialization() {
    let loc = Location {
        loc_start: Position { pos_fname: "test.res".into(), pos_lnum: 1, pos_bol: 0, pos_cnum: 0 },
        loc_end: Position { pos_fname: "test.res".into(), pos_lnum: 1, pos_bol: 0, pos_cnum: 5 },
        loc_ghost: false,
    };

    let mut w = MarshalWriter::new();
    loc.marshal(&mut w);
    let rust_output = w.finish();

    // Compare with OCaml output (generated separately and embedded as bytes)
    let ocaml_output = include_bytes!("testdata/location_marshal.bin");
    assert_eq!(rust_output, ocaml_output);
}
```

### Level 3: Integration Tests

Test complete binary AST generation:

```rust
#[test]
fn test_binary_ast_simple_module() {
    let source = r#"let x = 1"#;
    let ast = parse_structure(source);

    let mut output = Vec::new();
    write_structure_ast_to_vec(&mut output, "/test/simple.res", &ast);

    let expected = include_bytes!("testdata/simple.ast");
    assert_eq!(output, expected);
}
```

### Level 4: Comparison Tests

Generate `.ast` with both compilers and compare:

```bash
#!/bin/bash
# scripts/test_binary_ast_parity.sh

for file in tests/syntax_tests/data/parsing/grammar/**/*.res; do
    # Generate with OCaml
    ./packages/@rescript/darwin-arm64/bin/bsc.exe -bs-ast "$file" -o /tmp/ocaml.ast

    # Generate with Rust
    ./compiler-rust/target/release/bsc -bs-ast "$file" -o /tmp/rust.ast

    # Compare
    if ! diff /tmp/ocaml.ast /tmp/rust.ast; then
        echo "MISMATCH: $file"
        xxd /tmp/ocaml.ast > /tmp/ocaml.hex
        xxd /tmp/rust.ast > /tmp/rust.hex
        diff /tmp/ocaml.hex /tmp/rust.hex | head -50
        exit 1
    fi
done

echo "All files match!"
```

### Level 5: PPX Compatibility Tests

Verify PPXes can read Rust-generated binary ASTs:

```bash
# Generate AST with Rust
./compiler-rust/target/release/bsc -bs-ast test.res -o test.ast

# Run PPX on it (e.g., react PPX)
./ppx_react test.ast -o test_transformed.ast

# Verify transformed AST is valid
./packages/@rescript/darwin-arm64/bin/bsc.exe -bs-read-ast test_transformed.ast
```

### Test Data Generation

Create reference binary files from OCaml:

```ocaml
(* scripts/generate_test_data.ml *)

let () =
  let loc = {
    Location.loc_start = { Lexing.pos_fname = "test.res"; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 };
    loc_end = { pos_fname = "test.res"; pos_lnum = 1; pos_bol = 0; pos_cnum = 5 };
    loc_ghost = false;
  } in
  let oc = open_out_bin "location_marshal.bin" in
  output_value oc loc;
  close_out oc
```

---

## Risk Analysis

### High Risk

#### 1. Sharing Semantics Mismatch

**Risk**: OCaml's Marshal preserves object identity. If two AST nodes point to the same `Location.t` instance, it's serialized once and referenced via `CODE_SHARED*`. Rust doesn't have the same pointer identity semantics.

**Impact**: Non-identical output even with correct encoding logic.

**Mitigation**:
1. First, test if OCaml uses `No_sharing` for AST output (check `binary_ast.ml`)
2. If sharing is used, analyze patterns empirically
3. Consider building AST with explicit `Rc<T>` for shared structures
4. May need to process AST to match OCaml's sharing decisions

**Status**: Unknown - needs investigation

#### 2. Variant Tag Assignment

**Risk**: OCaml assigns block tags to non-constant constructors in declaration order, starting from 0. Getting this wrong breaks everything.

**Impact**: All variant serialization incorrect.

**Mitigation**:
1. Carefully document tag assignments for all types
2. Automated verification against OCaml
3. Code review by someone familiar with OCaml internals

**Status**: Manageable with careful implementation

### Medium Risk

#### 3. Integer Encoding Edge Cases

**Risk**: OCaml integers are 63-bit (on 64-bit platforms), but stored untagged in Marshal. Edge cases around `min_int`/`max_int` might differ.

**Impact**: Incorrect encoding for extreme values.

**Mitigation**:
1. Test all boundary cases
2. Use i64 internally, validate ranges

**Status**: Low probability, easy to test

#### 4. String Encoding

**Risk**: OCaml strings are bytes, Rust strings are UTF-8. Edge cases with non-UTF-8 data.

**Impact**: Encoding errors or panics.

**Mitigation**:
1. Use `Vec<u8>` internally for strings that might contain non-UTF-8
2. ReScript source is UTF-8, so this may not be an issue in practice

**Status**: Low probability

#### 5. Platform Differences

**Risk**: Marshal format includes platform-specific sizing (size_32, size_64 fields). May vary between platforms.

**Impact**: Non-identical output on different platforms.

**Mitigation**:
1. Compute sizes using fixed formulas (not platform-dependent)
2. Test on all supported platforms

**Status**: Needs verification

### Low Risk

#### 6. Float Encoding Endianness

**Risk**: Float encoding depends on platform endianness.

**Impact**: Wrong float values.

**Mitigation**:
1. Detect endianness at runtime
2. Use appropriate `CODE_DOUBLE_*` variant
3. ReScript rarely uses floats in AST (only in constants)

**Status**: Well-understood, easy to handle

#### 7. Header Size Calculation

**Risk**: Header contains object count and size predictions. Must match OCaml exactly.

**Impact**: Header mismatch.

**Mitigation**:
1. Track counts during serialization
2. Verify against OCaml output

**Status**: Mechanical, testable

---

## Open Questions

### Must Answer Before Implementation

1. **Does `binary_ast.ml` use `No_sharing`?**
   - ✅ **ANSWERED**: No, it uses `output_value oc pt` without flags
   - **Sharing IS enabled** (OCaml default)
   - This means we must replicate OCaml's sharing decisions exactly
   - See [Sharing Analysis](#sharing-analysis) below

2. **What is the exact tag assignment for all parsetree0 variants?**
   - Need to verify against OCaml documentation or source

3. **Are there any differences between parsetree and parsetree0?**
   - Check `ast_mapper_to0.ml` for the mapping logic
   - Some types may have been modified

4. **What platforms must we support?**
   - darwin-arm64, darwin-x64, linux-x64, win32-x64?
   - Any 32-bit platforms?

### Can Answer During Implementation

5. **How does OCaml handle the mutable `pexp_attributes` field?**
   - Does mutability affect serialization?

6. **Are there any dummy/placeholder AST nodes to handle specially?**
   - `Ptyp_class of unit`, `Pexp_object of unit`, etc.

7. **What's the maximum size of a typical AST?**
   - Affects whether we need streaming or can buffer in memory

---

## References

### OCaml Documentation

- [OCaml Marshal API](https://ocaml.org/api/Marshal.html)
- [OCaml Runtime Source - extern.c](https://github.com/ocaml/ocaml/blob/trunk/runtime/extern.c)
- [OCaml Runtime Source - intern.c](https://github.com/ocaml/ocaml/blob/trunk/runtime/intern.c)
- [OCaml Runtime Source - intext.h](https://github.com/ocaml/ocaml/blob/trunk/runtime/caml/intext.h)

### ReScript Codebase

- `compiler/depends/binary_ast.ml` - Binary AST read/write
- `compiler/ml/parsetree0.ml` - Frozen PPX-compatible AST
- `compiler/ml/parsetree.ml` - Current AST
- `compiler/common/ast_mapper_to0.ml` - Current → parsetree0 mapping
- `compiler/common/ast_mapper_from0.ml` - parsetree0 → current mapping
- `compiler/frontend/ast_extract.ml` - Dependency extraction

### Existing Research

- Analysis of OCaml Marshal format in this codebase (this document)
- Empirical analysis of `.ast` files (see research notes)

---

## Sharing Analysis

### Critical Finding

**OCaml's `binary_ast.ml` uses sharing** (calls `output_value oc pt` without `No_sharing` flag).

This means:
1. When the same OCaml value is referenced multiple times, it's serialized once
2. Subsequent references use `CODE_SHARED*` opcodes
3. To achieve byte-identical output, we must make identical sharing decisions

### How OCaml Sharing Works

OCaml's Marshal maintains a hash table mapping heap addresses to object indices:

```
During serialization:
1. Before writing any block, check if address is in table
2. If found: write CODE_SHARED* with the index
3. If not found: add to table, write the block normally
```

### Sharing in ReScript AST

Common sharing patterns in the AST:

1. **Location.none** - The "no location" sentinel is often shared
2. **Empty attributes** - `[]` for attributes appears everywhere
3. **Repeated identifiers** - Same `Longident.t` may appear multiple times

### Challenge for Rust

Rust doesn't have OCaml's pointer identity semantics:
- OCaml: `x == y` checks if same heap object
- Rust: No built-in object identity for most types

### Proposed Solutions

#### Option A: Analyze and Replicate OCaml's Sharing Pattern

1. Dump OCaml Marshal output with sharing info
2. Identify which objects are shared
3. Build Rust AST with explicit sharing (using `Rc<T>`)
4. Track `Rc` pointer addresses for identity

**Pros**: Most accurate
**Cons**: Complex, requires deep OCaml analysis

#### Option B: Share Nothing Initially, Add Sharing Incrementally

1. Implement without sharing first
2. Compare output with OCaml
3. Add sharing for specific cases as needed

**Pros**: Simpler to start
**Cons**: May require many iterations

#### Option C: Use Deterministic Sharing Rules

1. Define rules for what gets shared (e.g., "all Location.none values")
2. Implement sharing based on value equality, not identity
3. May not match OCaml exactly but might be close enough

**Pros**: Predictable
**Cons**: May not achieve byte-identical output

### Recommended Approach

Start with **Option B**:

1. **Phase 1**: Implement without sharing
2. **Phase 2**: Generate test cases, compare with OCaml
3. **Phase 3**: Analyze differences, identify sharing patterns
4. **Phase 4**: Add sharing for identified patterns
5. **Phase 5**: Iterate until byte-identical

### Tools for Sharing Analysis

Create a tool to analyze OCaml Marshal output:

```rust
// Tool: analyze_marshal
// Reads .ast file and reports sharing information

fn analyze_sharing(data: &[u8]) {
    // Parse Marshal format
    // Track all CODE_SHARED* references
    // Report which objects are shared and how often
}
```

Example output:
```
Sharing Analysis for test.ast:
  Total objects: 1234
  Shared objects: 45
  Most shared:
    - Object at offset 0x100 (Location.t): referenced 23 times
    - Object at offset 0x200 (empty list): referenced 15 times
    - Object at offset 0x250 (Longident.t "Js"): referenced 7 times
```

### Sharing in the Header

The Marshal header includes `obj_count` - the number of objects in the sharing table. This must match exactly:

```
Header byte 8-11: obj_count (big-endian u32)
```

If our sharing decisions differ, `obj_count` will differ, causing header mismatch.

---

## Appendix A: Complete Tag Assignment Reference

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
| virtual_flag | Virtual | 0 |
| virtual_flag | Concrete | 1 |
| override_flag | Override | 0 |
| override_flag | Fresh | 1 |
| closed_flag | Closed | 0 |
| closed_flag | Open | 1 |
| variance | Covariant | 0 |
| variance | Contravariant | 1 |
| variance | Invariant | 2 |

### Noloc.arg_label (mixed)

| Constructor | Encoding |
|-------------|----------|
| Nolabel | int 0 |
| Labelled(s) | Block(tag=0, [s]) |
| Optional(s) | Block(tag=1, [s]) |

### Longident.t (all non-constant)

| Constructor | Tag |
|-------------|-----|
| Lident(s) | 0 |
| Ldot(t, s) | 1 |
| Lapply(t1, t2) | 2 |

### constant (all non-constant)

| Constructor | Tag |
|-------------|-----|
| Pconst_integer(s, char_opt) | 0 |
| Pconst_char(i) | 1 |
| Pconst_string(s, delim_opt) | 2 |
| Pconst_float(s, char_opt) | 3 |

*(Continue for all parsetree0 types...)*

---

## Appendix B: Parsetree → Parsetree0 Transformations

The current parsetree has evolved from parsetree0. When mapping back, several transformations are required.

### JSX Transformations

**New JSX nodes → Function calls**

JSX in the current parsetree uses dedicated AST nodes:
- `Pexp_jsx_element(Jsx_fragment {...})`
- `Pexp_jsx_element(Jsx_unary_element {...})`
- `Pexp_jsx_element(Jsx_container_element {...})`

These must be transformed to function application with `[@JSX]` attribute:

```
// Current parsetree
<Comp prop="value">child</Comp>

// Parsetree0
Comp(~prop="value", ~children=[child], ())[@JSX]
```

### Arrow Type Arity

**Current**: `Ptyp_arrow { arg; ret; arity: int option }`

**Parsetree0**: When arity is present, wrap in `function$` type constructor:

```
// Current: (int, int) => int with arity=2
// Parsetree0: function$<(int, int) => int, [#Has_arity2]>
```

### Function Arity

**Current**: `Pexp_fun { ...; arity: int option; async: bool }`

**Parsetree0**:
- When arity is present, wrap in `Function$` constructor with `[@res.arity N]`
- When async is true, add `[@res.async]` attribute

### Await Expression

**Current**: `Pexp_await(expr)`

**Parsetree0**: Add `[@res.await]` attribute to the inner expression

### Partial Application

**Current**: `Pexp_apply { ...; partial: bool }`

**Parsetree0**: When partial is true, add `[@res.partial]` attribute

### Optional Record Fields

**Current**: Record patterns/expressions have `opt: bool` on each field

**Parsetree0**: Add `[@res.optional]` attribute to field's pattern/expression

### Operator Rewriting

Several operators are rewritten during mapping:

| Current | Parsetree0 |
|---------|------------|
| `->` | `\|.` (pipe) |
| `++` | `^` (string concat) |
| `!=` | `<>` (structural inequality) |
| `!==` | `!=` (physical inequality) |
| `===` | `==` (physical equality) |
| `==` | `=` (structural equality) |

### New Record Syntax

**Current**: Record fields use `{ lid; x: expr; opt: bool }`

**Parsetree0**: Standard `(Longident.t loc * expression)` tuples, with optional indicated by attribute

### Implementation Notes

The mapper in `ast_mapper_to0.ml` handles all these transformations. The Rust implementation must replicate this logic exactly.

Key source: `compiler/ml/ast_mapper_to0.ml` (685 lines)

---

## Appendix C: Test File Hex Dumps

### Empty Module

Source: `let _ = ()`

Expected `.ast` file (hex):
```
00000000: 00 00 00 01 0a 2f 55 73 65 72 73 2f 2e 2e 2e 2f  ...../Users/.../
00000010: 65 6d 70 74 79 2e 72 65 73 0a 84 95 a6 be ...   empty.res.....
```

*(Add more examples as implementation progresses)*

---

## Appendix D: Estimated Implementation Effort

### Code Volume Estimates

| Component | Lines of Code | Complexity |
|-----------|--------------|------------|
| Marshal writer (`marshal.rs`) | ~500 | High |
| Basic type serialization (`serialize.rs`) | ~300 | Medium |
| Core types (`types.rs`) | ~200 | Low |
| Parsetree0 type definitions (`parsetree0.rs`) | ~800 | Medium |
| Parsetree0 marshal implementations (`parsetree0_marshal.rs`) | ~1200 | High |
| Parsetree mapper (`mapper_to0.rs`) | ~600 | High |
| Dependency extraction (`deps.rs`) | ~150 | Low |
| File writer (`writer.rs`) | ~100 | Low |
| Sharing support (if needed) | ~300 | Very High |
| Tests | ~1000 | Medium |
| **Total** | **~5150** | |

### Risk-Adjusted Timeline

| Phase | Optimistic | Realistic | Pessimistic |
|-------|-----------|-----------|-------------|
| Phase 1: Marshal writer | 2 days | 4 days | 7 days |
| Phase 2: Basic types | 1 day | 2 days | 4 days |
| Phase 3: Parsetree0 types | 3 days | 5 days | 8 days |
| Phase 4: Parsetree mapper | 2 days | 4 days | 7 days |
| Phase 5: Dependencies | 0.5 days | 1 day | 2 days |
| Phase 6: Integration | 1 day | 2 days | 4 days |
| Phase 7: Sharing (if needed) | 3 days | 7 days | 14 days |
| Testing & debugging | 3 days | 5 days | 10 days |
| **Total** | **15.5 days** | **30 days** | **56 days** |

### Critical Path

1. Marshal writer (blocks everything)
2. Parsetree0 types (blocks serialization)
3. Parsetree mapper (blocks testing)
4. Sharing analysis (unknown scope)

### Recommended Order of Implementation

1. Start with Marshal writer - this is the foundation
2. Implement basic types and test against OCaml
3. Add parsetree0 types incrementally, testing each
4. Implement mapper transformations
5. Integrate and test end-to-end
6. Address sharing issues as they arise

---

## Document History

| Date | Author | Changes |
|------|--------|---------|
| 2026-01-19 | Research | Initial comprehensive plan |
