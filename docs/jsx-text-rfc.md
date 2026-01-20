# RFC: JSX Text Support

## Summary

This RFC proposes adding support for bare text content inside JSX elements, allowing developers to write `<p>Hello</p>` instead of `<p>{React.string("Hello")}</p>`.

## Motivation

Currently, ReScript requires explicit `React.string()` calls for text content in JSX:

```rescript
// Current syntax (verbose)
<p>{React.string("Hello world")}</p>
<div>{React.string("Welcome to ")}{name}{React.string("!")}</div>
```

This is verbose compared to standard JSX in JavaScript/TypeScript:

```jsx
// Standard JSX (what we want)
<p>Hello world</p>
<div>Welcome to {name}!</div>
```

## Enabling the Feature

JSX text support is an experimental feature that must be explicitly enabled.

### Via rescript.json

Add to your `rescript.json`:

```json
{
  "experimental-features": {
    "JsxText": true
  }
}
```

### Via compiler flag

Pass the flag directly to the compiler:

```bash
bsc -enable-experimental JsxText myfile.res
```

For development/testing with dune:

```bash
dune exec bsc -- -enable-experimental JsxText myfile.res
```

## Design

### New AST Node

A new expression type `Pexp_jsx_text` is added to `parsetree.ml`:

```ocaml
| Pexp_jsx_text of jsx_text

and jsx_text = {
  jsx_text_content: string;       (* The trimmed text content *)
  jsx_text_leading_space: bool;   (* Had whitespace before the text *)
  jsx_text_trailing_space: bool;  (* Had whitespace after the text *)
}
```

This node represents literal text content inside JSX elements, with whitespace metadata for accurate round-tripping.

### Parsing (res_core.ml)

The parser's `parse_jsx_children_2` function recognizes text tokens inside JSX elements. When tokens don't match known JSX child starters (`<`, `{`, etc.), they are accumulated as text until a delimiter is reached.

The `parse_jsx_text` function:

1. Captures `prev_end_pos` to include any leading whitespace the scanner skipped
2. Accumulates tokens until it hits `<` (new element/closing tag) or `{` (expression)
3. Extracts the raw source text between positions
4. Trims whitespace but records whether leading/trailing whitespace existed
5. Creates a `Pexp_jsx_text` node with whitespace metadata

Forbidden characters `>` and `}` produce error messages suggesting alternatives.

### Printing (res_printer.ml)

The printer outputs `Pexp_jsx_text` as bare text without any wrapping:

- No braces `{}`
- No `React.string()` call
- Just the literal text content

The printer uses the whitespace metadata to determine separators between JSX children:

```ocaml
let get_separator_between x y =
  match (x.Parsetree.pexp_desc, y.Parsetree.pexp_desc) with
  | ( Pexp_jsx_text {jsx_text_trailing_space = trailing},
      Pexp_jsx_text {jsx_text_leading_space = leading} ) ->
    if trailing || leading then Doc.space else Doc.nil
  | Pexp_jsx_text {jsx_text_trailing_space = trailing}, _ ->
    if trailing then Doc.space else Doc.nil
  | _, Pexp_jsx_text {jsx_text_leading_space = leading} ->
    if leading then Doc.space else Doc.nil
  | _ -> (* default separator *)
```

This ensures round-trip preservation: `<p>A <span>B</span> C</p>` parses and prints with correct spacing.

### Type Checking (ast_mapper_to0.ml)

During the mapping from new AST to old AST (before type checking), `Pexp_jsx_text` is transformed to `React.string("text")`:

```ocaml
| Pexp_jsx_text text ->
  (* Transform JSX text to React.string("text") *)
  let react_string_ident = {loc; txt = Longident.Ldot (Lident "React", "string")} in
  let string_const = Ast_helper0.Exp.constant ~loc (Pconst_string (text.jsx_text_content, None)) in
  apply ~loc ~attrs (ident react_string_ident) [(Asttypes.Noloc.Nolabel, string_const)]
```

This means:

- The type system sees `React.string("text")` and type-checks it normally
- No changes needed to the type checker
- The generated JavaScript is identical to explicit `React.string()` calls

### Reverse Mapping (ast_mapper_from0.ml)

When converting from old AST to new AST (e.g., for PPX output), the mapper detects `React.string("literal")` patterns in JSX children and converts them back to `Pexp_jsx_text`:

```ocaml
| Pexp_apply
    ( {pexp_desc = Pexp_ident {txt = Longident.Ldot (Lident "React", "string")}},
      [(Nolabel, {pexp_desc = Pexp_constant (Pconst_string (text, None))})] ) ->
  Some (Ast_helper.Exp.jsx_text ~loc text)
```

## Whitespace Handling

The implementation tracks whitespace metadata for accurate round-tripping:

- **Content**: The actual text is trimmed (leading/trailing whitespace removed)
- **Leading space flag**: Records if whitespace existed before the trimmed text
- **Trailing space flag**: Records if whitespace existed after the trimmed text

This allows the printer to reconstruct proper spacing between elements:

```rescript
// Input
<div>Hello <span>world</span> today</div>

// Parsed as:
// - "Hello" with trailing_space=true
// - <span>world</span>
// - "today" with leading_space=true

// Reprints correctly as:
<div>Hello <span>world</span> today</div>
```

## Limitations

1. **No HTML entity decoding**: Text like `&gt;` remains as `&gt;`, not `>`. Users must use `{">"}` for literal special characters.

2. **No string interpolation**: Text is treated as a plain string. For dynamic content, use expression syntax: `<p>Hello {name}!</p>` or `<p>{React.string(`Hello {name}!`)}</p>`.

3. **Forbidden characters**: `>` and `}` inside text produce parser errors with helpful suggestions.

## Examples

### Before (current syntax)

```rescript
<div>
  <h1>{React.string("Welcome")}</h1>
  <p>{React.string("This is a paragraph with ")}<strong>{React.string("bold")}</strong>{React.string(" text.")}</p>
</div>
```

### After (with this RFC)

```rescript
<div>
  <h1>Welcome</h1>
  <p>This is a paragraph with <strong>bold</strong> text.</p>
</div>
```

### Mixed content

```rescript
<p>Hello {userName}, you have {unreadCount->Int.toString} messages.</p>
```

## Development Commands

All development commands require the `-enable-experimental JsxText` flag.

### Parse and view AST

To see how JSX text is parsed, including whitespace metadata:

```bash
dune exec bsc -- -enable-experimental JsxText -dparsetree path/to/file.res -only-parse
```

Example output:

```
Pexp_jsx_text "Hello" (leading=false, trailing=true)
```

### Parse and reprint (round-trip test)

To verify that parsing and reprinting preserves the original formatting:

```bash
dune exec bsc -- -enable-experimental JsxText -only-parse -reprint-source path/to/file.res
```

### View transformed AST (with React.string)

To see how the JSX text transforms to `React.string()` calls:

```bash
dune exec bsc -- -enable-experimental JsxText -bs-jsx 4 -dparsetree path/to/file.res
```

### Full compilation

To compile and see the generated JavaScript:

```bash
dune exec bsc -- -enable-experimental JsxText path/to/file.res
```

## Implementation Checklist

- [x] Add `Pexp_jsx_text` to `parsetree.ml` with whitespace metadata
- [x] Add `Ast_helper.Exp.jsx_text` helper with `leading_space` and `trailing_space` params
- [x] Update parser (`res_core.ml`) to parse JSX text with whitespace tracking
- [x] Update printer (`res_printer.ml`) to print bare text with correct spacing
- [x] Update `res_parens.ml` to not wrap JSX text in braces
- [x] Update `ast_mapper_to0.ml` to transform to `React.string()`
- [x] Update `ast_mapper_from0.ml` to recognize `React.string()` patterns
- [x] Update all pattern matches (`ast_mapper.ml`, `printast.ml`, `ast_iterator.ml`, `depend.ml`, `res_comments_table.ml`, `typecore.ml`, `res_ast_debugger.ml`, `bs_ast_mapper.ml`, `analysis/src/Utils.ml`)
- [ ] Add syntax tests
- [ ] Add integration tests
- [ ] Filter out empty text nodes
- [ ] Consider whitespace normalization (collapse multiple spaces/newlines)

## Files Modified

| File                                        | Purpose                                         |
| ------------------------------------------- | ----------------------------------------------- |
| `compiler/ml/parsetree.ml`                  | AST definition with `jsx_text` record type      |
| `compiler/ml/ast_helper.ml`                 | Helper function to create `Pexp_jsx_text` nodes |
| `compiler/ml/ast_helper.mli`                | Interface for the helper function               |
| `compiler/ml/experimental_features.ml`      | Added `JsxText` experimental feature            |
| `compiler/ml/experimental_features.mli`     | Interface for experimental features             |
| `compiler/syntax/src/res_core.ml`           | Parser implementation with whitespace tracking  |
| `compiler/syntax/src/res_printer.ml`        | Printer with whitespace-aware separators        |
| `compiler/syntax/src/res_parens.ml`         | Parenthesization rules                          |
| `compiler/ml/ast_mapper_to0.ml`             | Transform to `React.string()` for type checking |
| `compiler/ml/ast_mapper_from0.ml`           | Reverse transform from `React.string()`         |
| `compiler/ml/ast_mapper.ml`                 | AST mapper support                              |
| `compiler/ml/ast_iterator.ml`               | AST iterator support                            |
| `compiler/ml/printast.ml`                   | Debug AST printing                              |
| `compiler/ml/depend.ml`                     | Dependency analysis                             |
| `compiler/ml/typecore.ml`                   | Type checking error for direct use              |
| `compiler/syntax/src/res_comments_table.ml` | Comment attachment                              |
| `compiler/syntax/src/res_ast_debugger.ml`   | S-expression debug output                       |
| `compiler/frontend/bs_ast_mapper.ml`        | Frontend AST mapper                             |
| `analysis/src/Utils.ml`                     | Analysis utilities                              |
| `rewatch/src/config.rs`                     | Added `JsxText` to rewatch config parsing       |
| `docs/docson/build-schema.json`             | JSON schema for `rescript.json`                 |

## Future Considerations

1. **HTML entities**: Could add `&amp;`, `&lt;`, `&gt;`, `&quot;` decoding
2. **Whitespace modes**: Could add options for whitespace handling (preserve, collapse, trim)
3. **Promotion to stable**: Once the feature is proven stable, it could be enabled by default
