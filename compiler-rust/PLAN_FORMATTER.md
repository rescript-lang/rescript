# Plan: OCaml-Compatible Format Module for Rust ML Printer

## Overview

This document describes the implementation plan for a Rust formatter that matches OCaml's Format module behavior for the ML printer. The goal is to achieve 100% parity with OCaml's pprintast.ml output.

## OCaml Format Module Semantics

### Box Types

OCaml's Format module has 4 box types that determine how line breaks are inserted:

| Box Type | OCaml Function | Format Syntax | Behavior |
|----------|---------------|---------------|----------|
| **h** (horizontal) | `open_hbox` | `@[<h>` | Never breaks - all on one line |
| **v** (vertical) | `open_vbox n` | `@[<v n>` | Always breaks at every break hint |
| **hv** (horizontal-or-vertical) | `open_hvbox n` | `@[<hv n>` | All on one line OR all broken (exclusive) |
| **hov** (horizontal-or-vertical packing) | `open_hovbox n` | `@[<hov n>` | Pack as much as fits, then break |

The default `@[<n>` or `open_box n` creates a "hov" box.

### Break Hints

| Syntax | Meaning |
|--------|---------|
| `@;` or `@ ` | Break hint: space if fits, newline+indent if not |
| `@,` | Cut: zero-width break hint |
| `@\n` | Force newline |

### Key Insight: Deferred Decision

The crucial insight is that OCaml's Format module **buffers output** and makes the horizontal/vertical decision **when the box is closed**, based on whether the content fits within the margin.

## Implementation Design

### Core Data Structures

```rust
/// Box types matching OCaml's Format module
#[derive(Clone, Copy, Debug)]
enum BoxKind {
    /// Horizontal - never break
    H,
    /// Vertical - always break at hints
    V,
    /// Horizontal-or-Vertical (exclusive choice)
    HV,
    /// Horizontal-or-Vertical (packing)
    HOV,
}

/// A break hint in the output stream
#[derive(Clone, Debug)]
enum Token {
    /// Raw string to output
    String(String),
    /// Break hint: (spaces_if_no_break, indent_offset_if_break)
    Break { nspaces: usize, offset: i32 },
    /// Open a box
    OpenBox { kind: BoxKind, indent: i32 },
    /// Close current box
    CloseBox,
    /// Force newline
    Newline,
}

/// A box containing tokens
struct Box {
    kind: BoxKind,
    indent: i32,
    tokens: Vec<Token>,
    /// Total size if printed horizontally (computed lazily)
    size: Option<usize>,
}

/// The formatter state
pub struct Formatter<W: Write> {
    out: W,
    margin: usize,
    /// Current column position
    col: usize,
    /// Stack of open boxes (innermost last)
    box_stack: Vec<Box>,
    /// Current indentation level
    indent_stack: Vec<usize>,
}
```

### Algorithm

1. **Buffering Phase**: As content is printed, tokens are added to the current (innermost) box.

2. **Box Close**: When a box is closed:
   - Calculate the total horizontal size of the box
   - If `size + current_col <= margin`: render horizontally
   - Otherwise: render according to box type (v, hv, hov)

3. **Rendering**:
   - **H box**: All breaks become spaces
   - **V box**: All breaks become newlines
   - **HV box**: Either all spaces OR all newlines
   - **HOV box**: Pack greedily - break only when necessary

### API Design

The Formatter provides a fluent API with macros for convenience:

```rust
impl<W: Write> Formatter<W> {
    /// Create a new formatter
    pub fn new(out: W) -> Self;

    /// Set margin (default 80)
    pub fn set_margin(&mut self, margin: usize);

    /// Print a string
    pub fn string(&mut self, s: &str);

    /// Print using format! syntax
    /// Usage: f.print!("{} = {}", name, value)
    pub fn print(&mut self, args: std::fmt::Arguments);

    /// Open a box: @[<kind n>
    pub fn open_box(&mut self, kind: BoxKind, indent: i32);

    /// Close current box: @]
    pub fn close_box(&mut self);

    /// Break hint: @; or @
    /// nspaces: spaces if not breaking
    /// offset: additional indent offset if breaking
    pub fn break_(&mut self, nspaces: usize, offset: i32);

    /// Space that may become break: @
    pub fn space(&mut self) { self.break_(1, 0); }

    /// Cut (zero-width break): @,
    pub fn cut(&mut self) { self.break_(0, 0); }

    /// Force newline: @\n
    pub fn newline(&mut self);

    /// Flush all pending output
    pub fn flush(&mut self);

    // Convenience methods
    pub fn open_hbox(&mut self) { self.open_box(BoxKind::H, 0); }
    pub fn open_vbox(&mut self, n: i32) { self.open_box(BoxKind::V, n); }
    pub fn open_hvbox(&mut self, n: i32) { self.open_box(BoxKind::HV, n); }
    pub fn open_hovbox(&mut self, n: i32) { self.open_box(BoxKind::HOV, n); }
}
```

### Macro for Convenient Usage

```rust
/// Macro for formatted printing, similar to OCaml's pp f "..." pattern
macro_rules! pp {
    ($f:expr, $($arg:tt)*) => {
        $f.print(format_args!($($arg)*))
    };
}

// Usage example:
fn print_let_binding(f: &mut Formatter<impl Write>, binding: &ValueBinding) {
    f.open_hovbox(2);       // @[<hov2>
    pp!(f, "let ");
    print_pattern(f, &binding.pvb_pat);
    f.space();              // @;
    pp!(f, "=");
    f.space();              // @;
    print_expression(f, &binding.pvb_expr);
    f.close_box();          // @]
}
```

## Migration Strategy

### Phase 1: Core Formatter Implementation

1. Implement `Formatter` struct with token buffering
2. Implement box management (open/close)
3. Implement break hint handling
4. Implement rendering algorithm for each box type

### Phase 2: Migrate Print Functions

Convert existing print functions from:
```rust
fn print_expression_ml(expr: &Expression, out: &mut impl Write)
```

To:
```rust
fn print_expression<W: Write>(f: &mut Formatter<W>, expr: &Expression)
```

Start with leaf functions and work up:
1. `print_constant`
2. `print_longident`
3. `print_pattern`
4. `print_core_type`
5. `print_expression` (and variants)
6. `print_structure_item`
7. `print_signature_item`

### Phase 3: Match OCaml Patterns

Translate OCaml format strings to Rust calls:

| OCaml | Rust |
|-------|------|
| `pp f "@[<2>let %a@]" ...` | `f.open_box(HOV, 2); pp!(f, "let "); ...; f.close_box();` |
| `pp f "@[<hv0>%a@]" ...` | `f.open_hvbox(0); ...; f.close_box();` |
| `pp f "%a@;%a" ...` | `...; f.space(); ...;` |
| `pp f "%a@,%a" ...` | `...; f.cut(); ...;` |
| `pp f "@\n" ...` | `f.newline();` |

## Implementation Details

### Size Calculation

For HOV boxes, we need to know if content fits on the current line. The size of a box is:

```rust
fn calculate_box_size(tokens: &[Token]) -> usize {
    let mut size = 0;
    for token in tokens {
        match token {
            Token::String(s) => size += s.len(),
            Token::Break { nspaces, .. } => size += nspaces,
            Token::OpenBox { .. } => {
                // Recursively get nested box size
                // (need to track matching CloseBox)
            }
            Token::CloseBox => { /* handled by nesting */ }
            Token::Newline => return usize::MAX, // Forces break
        }
    }
    size
}
```

### Rendering Algorithm

```rust
fn render_box(&mut self, box_: Box) {
    let fits = self.col + box_.size.unwrap_or(0) <= self.margin;

    match box_.kind {
        BoxKind::H => {
            // Always horizontal - breaks become spaces
            self.render_tokens(&box_.tokens, false);
        }
        BoxKind::V => {
            // Always vertical - breaks become newlines
            self.render_tokens(&box_.tokens, true);
        }
        BoxKind::HV => {
            // Exclusive: all horizontal OR all vertical
            self.render_tokens(&box_.tokens, !fits);
        }
        BoxKind::HOV => {
            // Packing: break only when necessary
            self.render_tokens_packing(&box_.tokens, box_.indent);
        }
    }
}

fn render_tokens(&mut self, tokens: &[Token], breaking: bool) {
    for token in tokens {
        match token {
            Token::String(s) => self.write_string(s),
            Token::Break { nspaces, offset } => {
                if breaking {
                    self.write_newline_indent(*offset);
                } else {
                    self.write_spaces(*nspaces);
                }
            }
            Token::OpenBox { kind, indent } => {
                // Recursively handle nested box
            }
            // ...
        }
    }
}
```

### Context Management

Like OCaml's pprintast.ml, we need context for parenthesization:

```rust
#[derive(Clone, Copy)]
struct Context {
    /// In a pipe context (|)
    pipe: bool,
    /// In a semicolon context (;)
    semi: bool,
    /// In an if-then-else context
    ifthenelse: bool,
}

impl Context {
    fn reset() -> Self { Context { pipe: false, semi: false, ifthenelse: false } }
    fn under_pipe(self) -> Self { Context { pipe: true, ..self } }
    fn under_semi(self) -> Self { Context { semi: true, ..self } }
    fn under_ifthenelse(self) -> Self { Context { ifthenelse: true, ..self } }
}
```

## Testing Strategy

1. **Unit tests**: Test each box type in isolation
2. **Integration tests**: Compare output with OCaml pprintast
3. **Parity tests**: Run `test_parser_ast_parity.sh` to verify output matches

## Files to Modify

| File | Changes |
|------|---------|
| `compiler-rust/src/parser/formatter.rs` | New file: Formatter implementation |
| `compiler-rust/src/parser/ml_printer.rs` | Rewrite to use Formatter |
| `compiler-rust/src/parser/mod.rs` | Export formatter module |

## Success Criteria

1. All 506 syntax tests pass (currently 51/506)
2. Output matches OCaml byte-for-byte for non-whitespace content
3. Line breaks occur at same positions as OCaml output

## References

- [OCaml Format Module Documentation](https://ocaml.org/manual/5.4/api/Format.html)
- [Format Tutorial](https://ocaml.org/manual/5.2/api/Format_tutorial.html)
- [Pretty Printing in OCaml](https://keleshev.com/pretty-printing-in-ocaml-a-format-primer)
- `compiler/ml/pprintast.ml` - Reference implementation
