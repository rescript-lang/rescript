# Rust Printer Rewrite Plan

This document outlines the plan for rewriting the Rust printer (`compiler-rust/src/parser/printer.rs`) to be a proper 1:1 port of the OCaml printer (`compiler/syntax/src/res_printer.ml`).

## Problem Statement

The current Rust printer is fundamentally broken:
- It writes directly to a string buffer instead of building `Doc` structures
- It doesn't follow the OCaml printer's structure
- It uses ad-hoc logic to try to match output behavior
- It doesn't handle comments properly via a CommentTable

The OCaml printer uses a sophisticated `Doc`-based pretty printing system that:
- Builds a document tree representing the output structure
- Renders the tree with proper line breaking based on a width constraint
- Handles comments by attaching them to AST nodes via a CommentTable

## OCaml Printer Architecture

### Key Components

1. **`Res_doc` module** (`res_doc.ml`)
   - Defines the `Doc.t` type for pretty printing documents
   - Types: `Nil`, `Text`, `Concat`, `Indent`, `IfBreaks`, `LineSuffix`, `LineBreak`, `Group`, `CustomLayout`, `BreakParent`
   - Provides `to_string ~width` for rendering

2. **`Res_comments_table` module** (`res_comments_table.ml`)
   - Maps locations to comments (leading, inside, trailing)
   - `walk_structure`/`walk_signature` populates the table from AST + comments
   - Comments are consumed as nodes are printed

3. **`Res_printer` module** (`res_printer.ml`)
   - ~6100 lines of OCaml
   - All print functions return `Doc.t`
   - Uses `State.t` for tracking custom_layout depth
   - Entry points: `print_implementation`, `print_interface`

### OCaml State Module

```ocaml
module State = struct
  let custom_layout_threshold = 2
  type t = {custom_layout: int}
  let init () = {custom_layout = 0}
  let next_custom_layout t = {custom_layout = t.custom_layout + 1}
  let should_break_callback t = t.custom_layout > custom_layout_threshold
end
```

### OCaml Print Function Signatures

All print functions follow this pattern:
```ocaml
val print_structure_item : state:State.t -> Parsetree.structure_item -> CommentTable.t -> Doc.t
val print_expression : state:State.t -> Parsetree.expression -> CommentTable.t -> Doc.t
(* etc. *)
```

### Key Helper Functions

1. **`print_comments`** - Wraps a Doc with leading/trailing comments
   ```ocaml
   let print_comments doc tbl loc =
     let doc_with_leading = print_leading_comments doc tbl.leading loc in
     print_trailing_comments doc_with_leading tbl.trailing loc
   ```

2. **`print_list`** - Prints a list of nodes with separators and comments
   ```ocaml
   let print_list ~get_loc ~nodes ~print ?(force_break=false) t = ...
   ```

3. **`print_lident`** / **`print_longident`** - Print identifiers with escaping

## Current Rust State

### Existing Infrastructure (Good)

1. **`doc.rs`** - Already a proper port of `Res_doc`
   - Has all the Doc types
   - Has `to_string` rendering
   - This can be reused

2. **`comment.rs`** - Comment type exists
   - Has `Comment` struct with `txt`, `style`, `loc`
   - Has `is_single_line()`, etc.

3. **`comment_table.rs`** - Just created (basic structure)
   - Needs `walk_structure`/`walk_signature` to populate

### What's Wrong (`printer.rs`)

1. **Writes to buffer instead of returning Doc**
   ```rust
   // WRONG - current approach
   fn print_structure_item(&mut self, item: &StructureItem) {
       self.write("let ");  // Direct string write
       ...
   }
   ```

2. **Mixed approaches** - Some parts use Doc, most don't

3. **Ad-hoc escaping logic** - Doesn't match OCaml's `classify_ident_content`

4. **No proper comment handling** - Comments aren't attached via CommentTable

## Current Status (2026-01-27)

**Test Results:** 142/506 total tests passing (28%)

### Recently Completed
- ✅ `PrinterState` struct with custom_layout tracking
- ✅ Core helper functions: `print_comments`, `print_list`, `print_listi`, etc.
- ✅ Identifier printing with proper escaping: `classify_ident_content`, `print_ident_like`
- ✅ Longident printing: `print_longident`, `print_lident`, `print_longident_location`
- ✅ Expression printing (most cases)
- ✅ Pattern printing (most cases)
- ✅ Type printing
- ✅ Structure/signature printing
- ✅ **Module expression printing** (`print_mod_expr`) - fully implemented
- ✅ **Module type printing** (`print_module_type`) - fully implemented
- ✅ Module functor parameter printing
- ✅ With constraints for module types
- ✅ Fixed constructor escaping (true, false, etc. no longer escaped)
- ✅ Fixed assert expression formatting
- ✅ **Blank line preservation** - Fixed `Doc::to_string` to only trim spaces, not newlines
- ✅ **Type attributes position** - Attributes now printed before `type` keyword
- ✅ **Type name escaping** - Reserved words properly escaped in type declarations
- ✅ **Record field escaping** - Reserved words properly escaped in record fields
- ✅ **Type extension printing** - `print_type_extension` implemented
- ✅ **Polyvariant escaping** - Fixed double `#` issue in exotic polyvariant names
- ✅ **Pipe before attributed constructors** - `| @attr Constructor` now has leading pipe
- ✅ **Constraint patterns in cases** - `(-3.14: float)` properly parenthesized
- ✅ **Expression block recursion** - Fixed infinite recursion for Pexp_letmodule etc.
- ✅ **Ternary expressions** - Now prints `cond ? a : b` instead of `if cond {...}`
- ✅ **Pexp_send** - Now prints `obj["prop"]` instead of `obj#prop`
- ✅ **Send-set operator** - `#=` now prints as `obj["prop"] = value`
- ✅ **Unit arguments** - `fn(())` now prints as `fn()`
- ✅ **Type constraints** - No longer wrapped in extra parens; `expr: type` not `(expr : type)`
- ✅ **First-class modules** - `module(M: S)` not `module(M: module(S))`
- ✅ **Type rec_flag** - `Nonrecursive` prints nothing, `Recursive` prints `"rec "`
- ✅ **Variant type printing** - Fixed extra space before constructors
- ✅ **Package type line breaking** - `with` constraints now break properly
- ✅ **Attribute spacing** - Multiple attributes now have spaces between them
- ✅ **Labeled argument escaping** - Reserved keywords escaped in type labels (`~\"let"`)
- ✅ **Private types** - Private flag now printed for abstract, record, variant, open types
- ✅ **Type constraints** - `constraint 'a = typ` now printed for type declarations
- ✅ **Argument punning** - `~foo=foo` now prints as `~foo`
- ✅ **Async functions** - `is_async` field in Pexp_fun now checked to preserve async keyword
- ✅ **Pipe-first spacing** - `->` operator no longer has spaces around it
- ✅ **Sequence constraint parens** - `(foo(): unit)` in sequences preserves parentheses
- ✅ **Pattern alias parens** - `(x as y) as z` and `(Foo | Bar) as x` properly parenthesized
- ✅ **Exception pattern parens** - `exception (A | B)` properly parenthesized
- ✅ **Or-pattern printing** - Nested or-patterns wrapped in parens, e.g. `Red | (Blue | Green)`
- ✅ **Ppat_type escaping** - `#...\"type"` exotic identifiers properly escaped
- ✅ **Record pattern constraint parens** - `{age: (age2: int)}` properly parenthesized
- ✅ **First-class module patterns** - `module(P: S)` printed correctly
- ✅ **Extension payloads** - `%raw("__GC")` now prints the payload
- ✅ **String.get not converted to bracket syntax** - `String.get(s, i)` preserved, only `Array.get/set` uses brackets
- ✅ **If-else chains** - `else if` now printed properly instead of `else { if ... }`
- ✅ **If-let expressions** - `if let Some(x) = foo() { ... }` chain support
- ✅ **Type parameter variance** - `+` (covariant) and `-` (contravariant) modifiers now printed

### Known Issues / TODO
- ❌ Comment attachment: Comments not properly attaching to nodes
- ❌ JSX printing needs work
- ❌ Force-break logic for multi-line constructs (preserving source formatting)
- ❌ Type parameter line breaking for long parameter lists
- ❌ Dict syntax sugar not preserved (prints as `Primitive_dict.make([])`)
- ❌ Some edge cases in expression printing

## Implementation Plan

### Phase 1: Infrastructure

1. **Complete `comment_table.rs`**
   - Add `walk_structure` function to populate table from AST + comments
   - Add `walk_signature` function
   - Port the comment attachment logic from OCaml

2. **Create `State` struct`**
   ```rust
   pub struct PrinterState {
       custom_layout: i32,
   }

   impl PrinterState {
       const CUSTOM_LAYOUT_THRESHOLD: i32 = 2;

       pub fn init() -> Self { Self { custom_layout: 0 } }
       pub fn next_custom_layout(&self) -> Self {
           Self { custom_layout: self.custom_layout + 1 }
       }
       pub fn should_break_callback(&self) -> bool {
           self.custom_layout > Self::CUSTOM_LAYOUT_THRESHOLD
       }
   }
   ```

### Phase 2: Core Print Functions

Port these helper functions first (they're used everywhere):

1. **`print_comments`** - Attach leading/trailing comments to Doc
2. **`print_list`** - Print list of nodes with separators
3. **`print_listi`** - Print list with index
4. **`print_longident`** / **`print_lident`** - Identifier printing
5. **`print_ident_like`** / **`classify_ident_content`** - Identifier escaping

### Phase 3: Expression Printing

Port expression printing functions in order of dependency:

1. `print_expression` - Main entry point
2. `print_expression_with_comments`
3. `print_binary_expression`
4. `print_unary_expression`
5. `print_operand`
6. `print_ternary_operand`
7. `print_braces`
8. `print_pexp_apply`
9. `print_arguments`
10. `print_argument`
11. ... (many more)

### Phase 4: Pattern Printing

1. `print_pattern`
2. `print_pattern_record_row`
3. `print_pat_or_expr_record`
4. ... etc.

### Phase 5: Type Printing

1. `print_typ_expr`
2. `print_object_field`
3. `print_poly_var_ident`
4. ... etc.

### Phase 6: Structure/Signature Printing

1. `print_structure` / `print_signature`
2. `print_structure_item` / `print_signature_item`
3. `print_value_bindings`
4. `print_type_declarations`
5. `print_module_binding`
6. ... etc.

### Phase 7: Specialized Printing

1. JSX printing (`print_jsx_*`)
2. Extension/attribute printing
3. Module type printing

## Function Mapping

Key functions to port (OCaml -> Rust):

| OCaml Function | Line | Rust Function |
|---------------|------|---------------|
| `print_implementation` | 6123 | `print_implementation` |
| `print_interface` | 6132 | `print_interface` |
| `print_structure` | 578 | `print_structure` |
| `print_structure_item` | 588 | `print_structure_item` |
| `print_value_bindings` | 696 | `print_value_bindings` |
| `print_expression` | 2992 | `print_expression` |
| `print_expression_with_comments` | 3079 | `print_expression_with_comments` |
| `print_pattern` | 2310 | `print_pattern` |
| `print_typ_expr` | 1709 | `print_typ_expr` |
| `print_comments` | 318 | `print_comments` |
| `print_list` | 322 | `print_list` |
| `classify_ident_content` | 399 | `classify_ident_content` |
| `print_ident_like` | 419 | `print_ident_like` |

## Testing Strategy

1. **Use existing syntax tests** - `PARSER=rust ./scripts/test_syntax.sh`
2. **Compare output character-by-character** with OCaml printer
3. **Focus on one category at a time** (e.g., all expr tests, then all type tests)
4. **Add regression tests** for any edge cases discovered

## Success Criteria

1. All printer syntax tests pass (currently 30/187)
2. Output is byte-for-byte identical to OCaml printer
3. Comments are preserved in correct positions
4. Line breaking behavior matches OCaml at default width (100)

## Notes

- The OCaml printer is ~6100 lines; expect Rust to be similar size
- Don't try to "improve" anything - exact 1:1 port first
- Keep the old printer code as reference (can delete after port is complete)
- The `Doc` module is already correct - just use it properly

## References

- OCaml printer: `compiler/syntax/src/res_printer.ml`
- OCaml doc: `compiler/syntax/src/res_doc.ml`
- OCaml comment table: `compiler/syntax/src/res_comments_table.ml`
- OCaml parens: `compiler/syntax/src/res_parens.ml`
- OCaml parsetree viewer: `compiler/syntax/src/res_parsetree_viewer.ml`
