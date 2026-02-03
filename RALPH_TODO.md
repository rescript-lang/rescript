# Syntax Parity TODO

**Last Updated:** 2026-02-03
**Overall Status:** 423/506 tests passing (83%)

**Category Breakdown:**
| Category | Passed | Failed | Total | Percent |
|----------|--------|--------|-------|---------|
| printer | 187 | 0 | 187 | 100% ✅ |
| ast-mapping | 3 | 0 | 3 | 100% ✅ |
| ppx/react | 31 | 0 | 31 | 100% ✅ |
| conversion | 27 | 0 | 27 | 100% ✅ |
| parsing/grammar | 93 | 42 | 135 | 68% |
| parsing/other | 12 | 2 | 14 | 85% |
| parsing/recovery | 14 | 6 | 20 | 70% |
| parsing/errors | 55 | 29 | 84 | 65% |
| parsing/infiniteLoops | 1 | 4 | 5 | 20% |

**Remaining:** 83 tests to fix

**Recent Fixes (this session):**
- **Added proper comma error handling in parameter and argument lists**: Added Grammar::ParameterList
  and Grammar::ArgumentList breadcrumbs to parse_parameters and parse_call_args. When an unexpected
  token (like `]`) is encountered in a comma-delimited list, now correctly generates "Did you forget
  a `,` here?" instead of "Did you forget a `)` here?". Added handle_record_field_comma helper for
  record field parsing. This matches OCaml's parse_comma_delimited_region behavior.
- **Fixed template literal pattern parsing for string interpolation errors**: Use start_pos from backtick
  when reporting string interpolation error, matching OCaml's error location (3:5-7 instead of 3:6-7).
  Skip all tokens then report one error, instead of reporting multiple errors. Use None delimiter in
  error case (OCaml: Pconst_string("", None)).
- **Fixed semicolon spacing in inline record constructor fields**: OCaml's record_declaration uses
  `sep:";@\n"` which places the semicolon AFTER each field. Changed to print ` ;` after each field,
  matching OCaml's output format.
- **Fixed error location span in parse_atomic_expr**: OCaml uses `prev_end_pos` for start and `end_pos`
  for end when reporting unexpected token errors. Rust was using `prev_end_pos` for both, causing spans
  like "1:10" instead of "1:10-11". Now correctly matches OCaml's error location format.
- **Fixed scanner to preserve start_pos when skipping bad characters**: When the scanner encounters an
  unknown character (like `$`), it reports an error and recursively scans. OCaml preserves the original
  `start_pos` from before the bad character for the returned ScanResult, but Rust was returning the
  recursive result directly. This caused error locations like "1:7" instead of "1:5-7".
- **Fixed exotic identifier handling to match OCaml**: For uppercase exotic identifiers, preserve the
  full `\"...\"` wrapper in the scanner. For exotic identifiers with linebreaks, include the `\"` prefix
  and actual newline character. Fixed `print_poly_var_ident` to just strip the leading backslash
  (not add extra quotes) since the identifier already has quotes.
- **Used Lident diagnostic in parse_lident functions**: Changed pattern.rs and typ.rs parse_lident
  functions to use Lident diagnostic instead of generic Message. This produces the correct
  context-sensitive error message: "I'm expecting a lowercase name like \`user or \`age\`".
- **Added ExprSetField breadcrumb for record field mutation**: When parsing `expr.field = value`,
  leave the ExprSetField breadcrumb before parsing the value expression. This enables the error
  message "It seems that this record field mutation misses an expression" to be displayed.
- **Fixed value path error recovery for trailing dot**: When parsing a value path like `n.R.` where
  the identifier after the last dot is missing, emit an error and recover by adding `_` as the
  missing identifier. Fixes the `id.res` error test.
- **Added keyword field error handling for record fields**: When a reserved keyword is used as a record
  field name (e.g., `{type: int}`, `{type: 1}`, `{type}`), emit appropriate error messages:
  - In types: "Cannot use keyword `X` as a record field name. Suggestion: rename it (e.g. `X_`).
    If you need the field to be "X" at runtime, annotate the field: `@as("X") X_ : ...`"
  - In expressions: "Cannot use keyword `X` as a record field name. Suggestion: rename it (e.g. `X_`)"
  - In patterns: "Cannot use keyword `X` here. Keywords are not allowed as record field names."
  If keyword is followed by colon, recover by renaming field to `X_`. Fixes 6 tests.
- **Added dict spread error message**: When `...` is used in a dict literal, emit "Dict literals do not
  support spread (`...`) yet." instead of the generic "Dict keys must be strings" error.
- **Added `let?` error messages for signatures and rec**: Added two checks matching OCaml:
  1. `let?` is not allowed in signatures - emit error and recover
  2. `let? rec` combination is not allowed - emit error and recover
- **Added second record spread error in expressions**: When a record has more than one `...` spread
  (e.g., `{...x, ...y}`), emit "Records can only have one `...` spread, at the beginning."
- **Added missing tilde error for labeled type parameters**: When parsing function type parameters
  like `(name: type)` without the tilde, emit "A labeled parameter starts with a `~`. Did you mean: `~name`?"
- **Fixed if-expression error recovery with regions/breadcrumbs**: Added proper `begin_region`/`end_region`
  and breadcrumbs (`ExprIf`, `IfCondition`, `IfBranch`, `ElseBranch`) to if-expression parsing.
  This matches OCaml's region-based error suppression: first error in then-branch silences subsequent
  errors in that region, but else-branch starts a new region so its errors are reported.
  Also fixed `parse_block_body` to check `is_block_expr_start` before continuing, which is critical
  for error recovery (prevents `else` from being parsed as a block expression).
- **Added spread pattern error messages for array and record patterns**: Implemented `parse_non_spread_pattern`
  helper that checks for `...` and emits "Array spread (`...`) is not supported in pattern matches"
  with the full explanation. Also added spread detection in record pattern parsing. This fixes
  error messages in tests like parsing/recovery/expression/list.res.
- **Added 'Did you mean ==?' error for = as binary operator**: When `=` is used as a binary operator
  (e.g., `if a = b { ... }`), emit "Did you mean `==` here?" error. This matches OCaml's
  `make_infix_operator` function which detects this common mistake.
- **Added string interpolation error in pattern matching**: When template literal patterns contain
  interpolation (`${...}`), emit "String interpolation is not supported in pattern matching" error.
- **Added type definition in function error**: When `type t = ...` is used inside a block/function,
  emit "Type definitions are not allowed inside functions. Move this `type` declaration to the
  top level or into a module." Added `Token::Typ` to `is_block_expr_start` and proper parsing
  to get the full span of the type definition for accurate error location.
- **Added external missing JS value name error**: When `external foo: T =` is not followed by a
  string, emit "An external requires the name of the JS value you're referring to, like \"foo\"."
- **Fixed empty inline record parsing**: Empty constructor arguments like `Node({})` were being
  parsed as object types (`< >`) instead of inline records (`{ }`). Removed `Token::Rbrace` from
  the object type detection heuristic. Also fixed ML printer indentation for empty inline records.
- **Fixed pattern error recovery with skip_tokens_and_maybe_retry**: When an unexpected token is seen
  where a pattern is expected, call `skip_tokens_and_maybe_retry` to try to find a valid pattern start
  token. For example, `let = 2` now recovers to `let 2 = [%rescript.exprhole]` matching OCaml (skips `=`,
  finds `2` as valid pattern start, parses `2` as pattern, then RHS is missing so returns exprhole).
- **Fixed switch case body to use exprhole for missing RHS**: When a switch case has a missing RHS
  (pattern parsed but no expression after `=>`), return `[%rescript.exprhole]` instead of `()`. This
  matches OCaml's behavior where `parse_expr_block` always tries to parse an expression, and
  `parse_atomic_expr` returns exprhole via `skip_tokens_and_maybe_retry` when the token is `}`.

- **Fixed type declaration error recovery**: When parsing `type` without a name (EOF or invalid token),
  create a placeholder type declaration with name `_` instead of producing nothing. This matches OCaml's
  `parse_lident` which returns `("_", loc)` for error recovery.
- **Added single-element tuple error**: Emit "A tuple needs at least two elements" when a tuple has
  only one element (detected by trailing comma). Added to all three contexts: pattern, expression,
  and type tuples. Fixed error location to use start_pos and prev_end_pos matching OCaml.
- **Added missing tilde labeled parameter error**: When a function parameter has `name=value` without
  a `~` prefix, emit "A labeled parameter starts with a `~`. Did you mean: `~name`?" and recover by
  treating it as an optional parameter.
- **Fixed Lident diagnostic message**: Match OCaml's typo in the error message for expected lowercase
  identifiers: "I'm expecting a lowercase name like `user or `age`" (missing backtick after "user").
- **Added parse_ident helper**: Replicate OCaml's parse_ident behavior which checks for keywords on
  same line before emitting custom error message. Used for type variable parsing after `'`.
- **Fixed type variable keyword error**: For `'let`, emit "`let` is a reserved keyword. Keywords
  need to be escaped: \"let\"" instead of generic type variable error.
- **Added skip_tokens_and_maybe_retry to type expression parsing**: When encountering an unexpected
  token in parse_atomic_typ_expr, try to skip tokens and retry parsing if we find a valid type
  expression start. This matches OCaml's error recovery behavior. Fixes garbage.res test.
- **Added MinusGreater error recovery in arrow types**: When -> is used instead of => in type
  expressions, generate "Did you forget a `=>` here?" and continue parsing. Matches OCaml's
  res_core.ml parse_arrow_type_rest behavior.
- **Added grammar context to expect(Colon) for type hints**: Pass Grammar::TypeExpression context
  when expecting a colon in labeled parameters and object fields, adding the hint "It signals
  the start of a type" to error messages.
- **Fixed type definition parsing - Bar before Equal**: When parsing type definitions, if Bar (`|`) is
  seen instead of Equal, call expect(Equal) to generate "Did you forget a `=` here?" with proper
  location span (from end of name to the Bar token).
- **Fixed type name parsing - Lident diagnostic**: When an uppercase identifier (like T1) is seen
  where a type name is expected, emit Lident diagnostic: "Did you mean `t1` instead of `T1`?".
  When a dotted path (like Foo.bar) is seen, emit: "A type declaration's name cannot contain a
  module access. Did you mean `bar`?"
- **Fixed type extension uppercase names**: When parsing type extension paths like M.T2, the final
  segment (the type name) should be lowercase. Emit Lident diagnostic for uppercase final segments.
- **Fixed exotic identifier error location**: When reporting errors for exotic identifiers (e.g.,
  empty string `\""` or linebreaks), the error location should start at the backslash, not the
  opening quote. Now reports `1:6-8` instead of `1:7-8`.
- **Fixed EOF handling in type name parsing**: When EOF is encountered in type name context, use
  Unexpected diagnostic instead of Lident. This generates the generic "I'm not sure what to parse
  here when looking at 'eof'" message matching OCaml.
- **Fixed infinite loop in signature parsing**: When `parse_signature_item` returns None for unexpected
  tokens (like `export type`), the parser now emits an "unexpected" error and advances the token, matching
  OCaml's `parse_region` behavior. Also added proper breadcrumbs: `Grammar::Specification` for file-level
  signatures and `Grammar::Signature` for braced signatures (inside module types). The key difference is
  that `Signature` terminates on `Rbrace` but `Specification` doesn't.
- **Added PatternMatchCase and Pattern breadcrumbs for switch/try cases**: OCaml sets PatternMatchCase
  breadcrumb before parsing each case, and Pattern breadcrumb before parsing the pattern. This enables
  proper context-aware error messages like "I was expecting a pattern to match on before the `=>`".
- **Fixed catch token handling**: Changed from manual `Message("Expected 'catch'")` to using
  `expect(Token::Lident("catch"))` to get proper "Did you forget a `catch` here?" error message.
- **Added sigitemhole recovery for signature items**: When attributes exist but no valid signature item
  follows, OCaml reports "Did you forget to attach X to an item?" and returns a sigitemhole placeholder.
  Added `default_signature_item()` and error handling in `parse_signature_item` to match.
- **Moved parse_newline_or_semicolon_structure inside structure item branches**: OCaml calls
  parse_newline_or_semicolon_structure INSIDE each branch of parse_structure_item_region, before
  calling end_region(). This allows errors to be reported within each item's region. For example,
  `export type t = int and export s = string` now correctly emits 2 errors (one for `export type`,
  one for `and export`) instead of just 1.
- **Added parse_newline_or_semicolon_signature for signature item parsing**: Similar fix for
  signature items, with proper regions for Let, Typ, and Module branches matching OCaml.
- **Propagated scanner diagnostics to parser**: Scanner errors like unclosed strings were being
  collected but never propagated to the parser's diagnostic list. Now scanner diagnostics are
  taken and added to parser diagnostics after each scan operation.
- **Added skip_tokens_and_maybe_retry for error recovery**: Implemented OCaml's error recovery
  mechanism that skips unexpected tokens and retries parsing. For example, `let foo = '2` now
  correctly parses as `let foo = 2` (recovering after the invalid `'` character).
- **Added Implementation breadcrumb to parse_structure**: OCaml's `parse_implementation` uses
  `parse_region` with `Grammar::Implementation` as the breadcrumb. This enables proper
  `should_abort_list_parse` checks during error recovery.
- **Added error for empty exotic identifiers**: When scanning exotic identifiers like `\""`,
  emit "A quoted identifier can't be empty string" error and preserve the full escaped form.
- **Fixed error reporting to match OCaml's region-based silencing**: Three key changes:
  1. Removed per-item regions in parse_structure - OCaml does NOT create a new region for each
     structure item. The region is shared so that after one error, subsequent errors are silenced.
  2. Changed err_multiple to err_at in parse_newline_or_semicolon_structure to respect regions.
  3. Replaced "Unexpected token: {:?}" messages with err_unexpected() to use proper Unexpected
     diagnostic category which runs through explain_unexpected() for context-aware messages.
- **Added begin_region/end_region to type and module parsing**: OCaml uses regions around specific
  parsing operations (Token::Typ and Token::Module) to allow errors within one definition to be
  de-duplicated while still reporting errors from different definitions.

- **Fixed item_attribute line-breaking in ML printer**: Added HOV box with break hint after attribute name,
  matching OCaml's "@[<2>[@@%s@ %a]@]" format. This enables proper line breaking for long attributes.
- **Fixed polyvariant line-breaking in ML printer**: Added proper box structure with break hints for
  polyvariant types - outer box for entire type, inner boxes for each row field, use break_(-2) for
  `|` separator to align with opening `[`.
- **Added specific error for dangling attrs/mutable in record fields**: When attributes or doc comments
  appear without a field name, OCaml gives "Attributes and doc comments can only be used at the beginning
  of a field declaration". Same for dangling `mutable` keyword. Fixed error location to use p.end_pos.
- **Added error recovery for = instead of : in record fields**: When user writes `{foo=string}` instead
  of `{foo: string}`, emit "Record fields in type declarations use `:`. Example: `{field: string}`"
  and continue parsing as if `:` was used.
- **Fixed empty record printing in ML printer**: When a record type has zero fields, the closing `}` was not
  being printed because the loop over fields never executed. Added special case for empty records.
- **Fixed error location spans in expect_with_grammar**: OCaml's `expect` uses `prev_end_pos` for start and
  `end_pos` for end, creating a span from previous token to current. Rust was using `prev_end_pos` for both.
  This fixes error location format like `:1:15-2:3` instead of just `:1:15`.
- **Implemented context-aware error messages in explain_unexpected**: The `explain_unexpected` function now properly
  uses the breadcrumbs (parsing context) to generate context-specific error messages matching OCaml's behavior.
  Fixed breadcrumb order issue - OCaml prepends to list (most recent at head), Rust appends (most recent at end).
  Now "I'm missing a type here" shows correctly for missing types in record field declarations.
- **DiagnosticCategory now uses Grammar enum** instead of strings for context (matching OCaml's res_diagnostics.ml):
  - `DiagnosticCategory::Unexpected.context` changed from `Vec<(String, Position)>` to `Vec<(Grammar, Position)>`
  - `DiagnosticCategory::Expected.context` changed from `Option<String>` to `Option<Grammar>`
  - `explain_unexpected` function can now pattern-match on `Grammar` variants directly
  - Added `Serialize`/`Deserialize` derives to `Grammar` enum
  - This enables proper context-aware error messages matching OCaml's breadcrumb-based diagnostics
- Empty polyvariant `[]` now generates TWO type holes (matching OCaml)
- Added begin_region/end_region around structure items so each can report errors
- Added polyvariant type error generation for error recovery mode:
  - `parse_row_fields_with_required_first()` requires at least one row field for `[< ...]` and `[...]`
  - `is_bar_or_doc_comment_then_bar()` handles doc comments before leading `|`
  - Generates "I'm not sure what to parse here" errors for invalid syntax like `[< ]` and `[]`
  - Recovered AST includes `[%rescript.typehole]` placeholders
- ML printer: add pipe context for match cases - wraps Pexp_fun/match/try/sequence in parens when
  used as match case RHS (matching OCaml's under_pipe context)
- ML printer: use simple_expr for record field values (matching OCaml's longident_x_expression)
- ML printer: wrap alias types (Ptyp_alias) in parens when used as arrow arguments
- Parser: accept uppercase type variables ('X) in type constraints, not just lowercase ('a)
- Added consecutive statements/expressions error checking (`parse_newline_or_semicolon_expr_block`,
  `parse_newline_or_semicolon_structure`) - detects missing ';' or newline between consecutive items
- Fixed ML printer Unicode escaping: non-ASCII bytes now escaped as decimal sequences (e.g., \226\156\133)
- Changed ML printer margin from 80 to 78 to match OCaml's Format module default
- Added HOV box with break hints to Ptyp_arrow for proper line wrapping at ->
- Fixed ML printer expression tier hierarchy (expression2 vs simple_expr)
- Fixed constrained pattern printing in value bindings (use simple_pattern for inner, add outer parens)
- Added pattern_is_simple function matching OCaml's simple_pattern behavior
- Fixed constructor attribute printing in type declarations (e.g., `@as(null)`)
- Fixed cons pattern parens in constraint patterns (don't wrap cons when not at binding top-level)
- Added trailing space after Ppat_unpack pattern (`(module Set) `)
- Added Pexp_send to simple expressions
- Fixed ML printer: add spaces between JSX children (matching OCaml's list separator)
- Fixed ML printer: avoid double parens in await expressions with attributed arguments
- Fixed ML printer: include `res.await` attribute in printed output (not filtered as internal)
- Fixed ML printer: record pattern punning only for simple Lident fields (not qualified Ldot)
- Added parens for alias/or patterns in record field patterns (pattern1 context)
- Fixed ML printer: complete list literals are now simple expressions (no extra parens around `[1; 2; 3]`)
- Implemented full JSX printing in ML printer (fragments, unary, container elements with props)
- Fixed package type constraint printing to use 'and' for subsequent constraints
- Fixed record field semicolon spacing when field has attributes
- Fixed type constraints in type declarations (ptype_cstrs)
- Fixed recursive module printing with constraint sugar and attributes
- Fixed module functor sugar (`module F(A:X) = ...`) and module application printing (`(F)(A)`)
- Fixed unit functor printing (`functor () ->` instead of `functor (*) ->`)

**Root Causes of Remaining Failures (144 tests):**

1. **parsing/grammar (47 failing)**: Almost entirely 80-column line wrapping differences.
   OCaml's Format module wraps long lines at 80 columns with specific indentation rules.
   The Rust Formatter doesn't match this behavior exactly. The actual AST content is correct.
   Note: Adding HOV boxes to item_attributes for proper line-breaking caused other regressions
   (different break points chosen in complex nested structures). Needs deeper investigation of
   how OCaml's Format module chooses break points.

2. **parsing/errors (74 failing)**: Multiple issues:
   - Error message wording differs ("I'm missing a type here" vs "Unexpected token")
   - Error location format differs (`:16-18` vs `:16`) - OCaml tracks start AND end pos for errors
   - OCaml's breadcrumb-based error messages provide context-specific hints
     **PARTIALLY FIXED**: DiagnosticCategory now uses Grammar enum, enabling pattern matching on
     breadcrumbs. Need to implement `explain_unexpected` logic matching OCaml's res_diagnostics.ml
   - Error recovery produces different recovered AST structures
   - OCaml's `skip_tokens_and_maybe_retry` advances past errors more aggressively
   - `_` as expression: OCaml detects `_` in non-function-argument context and generates error,
     replacing with `[%rescript.exprhole]`. Rust keeps `_` as-is. Complex to fix because
     need to distinguish `foo(_, bar)` (valid partial application) from `_ + 1` (invalid).

3. **parsing/recovery (16 failing)**: Error recovery output differs from OCaml.
   Both error messages and recovered AST formatting differ.

4. **parsing/infiniteLoops (4 failing)**: Actually doesn't loop infinitely, but error messages
   and AST output format differ from OCaml.

5. **parsing/other (3 failing)**: Mixed issues:
   - Error message differences
   - Line wrapping differences

**Note:** Most non-printer failures are NOT simple formatting issues - they require implementing
missing parser error checking logic (for "consecutive statements" errors) or matching OCaml's
exact Format module line-breaking algorithm.

---

## ⚠️ CRITICAL: Format Module Must Be 1:1 Port from OCaml

**The Rust formatting code is NOT an "emulation" - it must be a 1:1 PORT of OCaml's Format module.**

Relevant Rust files:
- `compiler-rust/src/parser/format.rs` - Basic pretty printer with boxes
- `compiler-rust/src/parser/doc.rs` - Document-based formatting
- `compiler-rust/src/parser/formatter.rs` - Formatting logic

When there are line-breaking or formatting differences:
1. **Look at the OCaml Format module source code** (`stdlib/format.ml` in the OCaml repo)
2. **Compare the algorithm step-by-step** with the Rust implementation
3. **Make the Rust code match OCaml exactly** - same box types, same break decisions, same indentation rules

Common issues that indicate Format parity problems:
- Different line break points (OCaml wraps at 78 columns by default)
- Different indentation levels after line breaks
- HOV boxes packing differently than expected
- Break hints being ignored or applied differently

**Do NOT try to "fix" or "improve" the algorithm** - match OCaml's behavior exactly, even if it seems suboptimal.

Reference: https://github.com/ocaml/ocaml/blob/trunk/stdlib/format.ml

---

## ⚠️ CRITICAL: Complete ALL Fixes, Not Just Easy Ones

**The goal is 100% parity with the OCaml reference implementation.** There is no reason to stop at "low-hanging fruit" or avoid complex features. Every remaining test must eventually pass, so tackle the hard problems now rather than deferring them.

**What this means:**
- **Spread array syntax** (`[...xs, a, b]`) - Implement the full reconstruction from `Belt.Array.concatMany`
- **Underscore apply rewriting** - DONE: Implemented `print_underscore_apply` and `print_underscore_apply_in_pipe`
- **Template literals** - Implement complete template literal printing
- **Any other missing feature** - If a test requires new infrastructure, build that infrastructure

**Do NOT:**
- Skip tests because they're "too complex"
- Move on to easier tests when stuck on a hard one
- Assume someone else will fix the difficult parts later

**The work is not done until ALL syntax tests pass (506/506).** Printer parity is complete (187/187), but parsing/errors, parsing/grammar, ppx, and conversion tests still need work. Study the OCaml implementation and implement equivalent Rust code.

---

### Recent Progress
- Implemented spread array/list syntax printing (array.res now passes):
  - Added `is_spread_belt_array_concat` and `is_spread_belt_list_concat` to detect Belt.Array/List.concatMany
  - Added `print_belt_array_concat_apply` and `print_belt_list_concat_apply` to print `[...xs, a, b]` syntax
  - Added special comment handling in comment_table for spread arrays/lists
- Fixed polyvariant row field comment handling (variant.res now passes):
  - Wrap tag_doc with print_comments using label.loc to attach leading/trailing comments
- Fixed template literal printing (templateLiteral.res now passes):
  - Implemented `print_template_literal` to reconstruct `foo ${bar}` from string concatenation
  - Fixed `print_string_contents` to split strings on newlines and join with `Doc::literal_line()`
  - This ensures proper position tracking for multiline strings (especially in template literals)
- Fixed JSX fragment arrow function placement (jsx.res now passes):
  - Added `Pexp_jsx_element(JsxElement::Fragment(_))` to `should_indent` check in arrow function
  - Now `let f = el => <> ... </>` keeps the fragment on the same line as `=>`
- Fixed doc comment inline spacing in type expressions (DocComments.res now passes):
  - Added `print_doc_comments_with_sep` with configurable separator (defaults to hard_line)
  - Use `Doc::space()` separator for doc comments on types to keep them inline
  - Remove extra spaces around doc comment content: use `/**` and `*/` directly
- Fixed binary.res printing (now passes):
  - Right-associative operators (exponentiation `**`): Added `is_rhs_binary_operator` function and
    flip the `is_lhs` parameter for these operators so `2. ** 3. ** 2.` and `(2. ** 3.) ** 2.`
    are parenthesized correctly
  - Single pipe expressions: Added `is_single_pipe_expr` function and `custom_layout` handling in
    `print_value_binding` so `let x = switch z {...}->switch y {...}` prints on one line
  - Attributed operands in flattening: When a binary operand has printable attributes, add outer
    parens (e.g., `a && @attr b && c` → `a && (@attr b) && c`). Added `flatten_operand_rhs_without_attrs`
    to handle the case where we've already partitioned printable attrs
- Fixed polyvariant tuple printing (polyvariant.res now passes):
  - When a polyvariant constructor has a tuple with a single element that is itself a tuple,
    e.g. `#Some((a, b))`, OCaml prints it hugged without indentation
  - Added special case matching OCaml's `Pexp_tuple [({pexp_desc = Pexp_tuple _} as arg)]`
- Fixed switch/match case comment handling (switch.res now passes):
  - Node::Case.get_loc() now uses braces attribute location if present for consistent key lookup
  - print_cases uses braces attribute location for full_loc and prev_end_line
  - This ensures trailing comments on same line as `}` appear correctly
  - Leading comments before case patterns no longer cause extra blank lines
- Fixed attribute positioning in pipe expressions inside binary operators (asyncAwait.res now passes):
  - When a binary expression with printable attributes appears as an operand of another binary operator
    (e.g., `(@foo (server->start))->foo`), the attributes should be printed outside the expression
  - Added `partition_printable_attributes` function to parsetree_viewer.rs
  - In `flatten_binary_operand`, for non-flattenable binary expressions: partition printable attrs,
    print the expression WITHOUT printable attrs (using print_binary_expression directly), wrap in
    parens if needed for precedence, then prepend the printable attrs outside
- Fixed ternary expression indentation in value bindings (ternary.res now passes):
  - When ternary condition is a binary expression or has attributes, indent the ternary on a new line
  - Updated `should_indent` logic in `print_value_binding` to match OCaml's handling
  - Added `has_tagged_template_attr` function for special handling of tagged templates
- Fixed binary expression flattening (binaryExpr.res now passes):
  - Implemented `flatten_binary_operand` function matching OCaml's `flatten` logic in `print_operand`
  - When printing a binary operand that is itself a flattenable binary expression (same precedence,
    no attributes), recursively flatten instead of nesting with group/indent
  - This prevents excessive indentation for chains like `a && b && c && d`
  - Added `print_binary_operator_with_spacing` helper matching OCaml's `print_binary_operator`
  - Handles special cases: setfield (parens only on LHS), #= operator, await expressions
- Fixed PPat payload printing (attributes.res now passes): use `if` instead of `when` for guard,
  wrap in indent with soft_line matching OCaml's format
- Fixed typexpr.res comment handling (now passes):
  - Fixed Ptyp_package constraint comments: use `make_combined_pos_range` and `print_comments_by_pos`
    to wrap each package constraint with comments using a location spanning from name to type end
  - Fixed Ptyp_poly type variable comments: use `visit_list_but_continue_with_remaining_comments`
    to walk type variables, attaching leading/trailing comments to each variable's location
  - Fixed empty record type with comments: handle `fields.is_empty()` case in Ptype_record by
    printing `{ print_comments_inside(...) }` instead of calling `print_record_declaration`
  - Fixed labeled arrow type parameter comments: OCaml uses type's location for partitioning but
    combined location for `get_loc`. Comments between label and type (like `~a: /* c */ typ`)
    become "before" comments attached as leading to the type
  - Fixed Ptyp_object field comments: wrap label doc with `print_comments` for `label.loc`,
    then wrap entire field with `print_comments_by_pos` using combined location (label to type end)
- Fixed value binding sugar comment handling (valueBindingSugar.res now passes):
  - OCaml's comment_table.ml has special handling for value binding sugar forms where pattern
    and expression have the same location (e.g., `let x: type t. ... = (type t) => ...`)
  - Added `rewrite_value_binding_locs` function to adjust pattern location to end at the type
    annotation, and use the inner expression (from Pexp_constraint) instead of outer Pexp_newtype
  - This ensures comments for the function parameters are properly attributed to the expression,
    not consumed by the pattern
  - Also fixed Ptyp_arrow comment walking: now uses `parsetree_viewer::arrow_type` to flatten
    the arrow type into a list of parameters before walking, matching OCaml's approach
- Added value binding sugar printing in printer2.rs:
  - When pattern is `Ppat_constraint(pat, Ptyp_poly)` and expression is `Pexp_newtype`,
    print the sugared form `let x: type t. T = expr` instead of desugared form
- Fixed stack overflow in arrow_type when arity is 0:
  - The Rust code was checking `remaining_arity == 0` but OCaml checks `max_arity < 0`
  - When external declarations have labeled @as parameters with arity=0, Rust returned immediately
    with empty args and the original arrow type as return_type, causing infinite recursion
  - Changed remaining_arity from usize to isize and check `< 0` like OCaml
  - This fixes crashes on files like `printer/other/attributes.res`
- Fixed JSX comment handling in comment_table.rs (printer/comments/jsx.res now passes):
  - Implemented proper JSX comment attachment logic matching OCaml's res_comments_table.ml
  - For unary elements: calculate closing token location (/>), partition comments between tag name
    and next token (first prop or />), attach comments appropriately
  - For container elements: handle opening > token comments, children comments, and inside comments
    for empty containers with space between opening and closing tags
  - Use `partition_adjacent_trailing_before_next_token_on_same_line` for JSX tag name comments
- Implemented JSX printing in Rust printer:
  - Added `print_jsx_element` with support for fragments, unary, and container elements
  - Added `print_jsx_name` for lower, upper, and qualified tag names
  - Added `print_jsx_prop` for punning, value, and spreading props
  - Added `print_jsx_children` with proper line separation
  - Fixed JSX parser to properly populate `closing_tag` field (was always None)
- Added `has_comment_below` function and fixed simple pipe (->) comment handling:
  OCaml only adds `soft_line` before `->` when `has_comment_below` returns true (checking if the first
  trailing comment starts on a line BELOW the expression's end). This ensures `compilation // after`
  followed by `/* below */` on the next line followed by `->Plugin.buildAssets` prints as
  `compilation->Plugin.buildAssets // after\n/* below */ // trailing` (comments at end of line).
- Fixed doc comment partition to only include res.doc, not ocaml.doc (valueBinding.res almost passes):
  OCaml only treats "res.doc" as a doc comment (printed as /** ... */). "ocaml.doc" is a regular
  attribute and should stay on the same line with other attributes, not get a hard_line.
- Fixed binary expression indentation using should_indent_binary_expr (jsObjectAccess.res passes):
  OCaml's should_indent_binary_expr checks if LHS is a same-precedence sub-expression. Added
  flattenable_operators and same_precedence_sub_expression helpers to properly determine when
  the RHS of a binary expression should be indented.
- Fixed async detection for functions with leading (type a): The `is_async` flag was only checked
  on the top-level expression, but for `async (type a, ()) => body`, the Pexp_newtype is outermost.
  Now we check `is_async` on the FIRST Pexp_fun encountered, not just the top-level.
- Fixed newtype collection across function boundaries: OCaml's `fun_expr` only collects Pexp_newtype
  at the BEGINNING, not during the collect_params loop. This means `(type a, ()) => (type b c, x) => 3`
  keeps the two functions separate, not merging all newtypes into one.
- Fixed await expression to ignore braces for simple expressions: OCaml filters out `@res.braces`
  before checking if parens are needed, so `await {x}` prints as `await x`.
- Implemented special callback printing for functions in last/first arg position (expr.res passes):
  When a callback has a trailing comment between parameters and `=>` like `f(() // c1 => 1)`,
  OCaml moves the comment AFTER the entire function call: `f(() => 1) // c1`. Implemented:
  - `print_pexp_fun` with `in_callback` parameter that adds `soft_line` after return expression
  - `print_arguments_with_callback_in_last_position` using `custom_layout` for different formats
  - `print_arguments_with_callback_in_first_position` for callbacks as first argument
  - Uses `CommentTable::copy` to handle printing the same subtree multiple times
- Fixed attribute printing on binary expressions: `@ann (x->foo)` was missing the `@ann` attribute
  because the code only checked for printable attrs to add parens but never printed them.
- Fixed attribute printing in Pexp_apply special cases: Array.get, Array.set, and #= (send-set)
  were relying on the generic attribute handler which was disabled for Pexp_apply. Added explicit
  attribute printing to each special case.
- Fixed attribute duplication on function expressions: `@att x => 34` was printing as
  `@att @att (@att x) => 34`. Two issues fixed:
  1. `fun_expr` was including `pexp_attributes` in each `FunParam::Parameter.attrs`, but OCaml
     strips attributes by passing `{expr_ with pexp_attributes = []}` to `collect_params`.
  2. `print_expression` was always printing `e.pexp_attributes` at the end, but some expression
     types (Pexp_fun, Pexp_apply, etc.) already print their own. Added `should_print_its_own_attributes`
     check matching OCaml to skip double printing for those types.
- Fixed curried function printing: `(a, b, c) => (d, e, f) => 4` was being collapsed into
  `(a, b, c, d, e, f) => 4`. The issue was that `fun_expr` was collecting ALL nested function
  parameters without checking the `arity` field. OCaml's `fun_expr` has the condition
  `arity = None || n_fun = 0` - it only collects params from nested functions if there's no
  arity marker. Added this check to stop collecting when an arity marker is present.
- Fixed JS object set parenthesization in binary expressions (jsObjectSet.res passes): Similar to
  setfield, the `#=` operator only needs parens when on the LHS of a binary expression. For
  `(node["left"] = value)->pipe`, the parens around the assignment are now preserved.
- Fixed type extension attribute line breaking (typeExtension.res passes): Pass the path location
  to print_attributes_with_loc so that attributes on a separate line preserve the line break.
  `@attr\ntype t +=` now correctly prints with a line break between the attribute and the type.
- Fixed setfield parenthesization in binary expressions (setfield.res passes): OCaml's print_operand
  handles Pexp_setfield specially - it only needs parens when on the LHS of a binary expression,
  not when on the RHS. This means `a->@attr user.name = "steve"` doesn't need parens around the RHS.
  Added special case in binary_operand_needs_parens to bypass the general binary_expr_operand check.
- Fixed recursive module constraint printing (recModules.res passes): Handle Pmod_constraint
  in print_rec_module_bindings to print constraint before equals sign.
- Fixed try expression body parenthesization (try.res now passes): Match OCaml's printer which
  uses parens::expr to check if the try body needs braces.
- Fixed Array.get/Array.set formatting for complex index expressions (arrayGet.res, arraySet.res now pass):
  Match OCaml's printer which wraps non-trivial index expressions with soft_line and indent.
  For simple expressions (constants, identifiers), the index is printed inline.
- Added special case for Array.get/Array.set in comment_table.rs: Match OCaml which walks
  argument expressions directly instead of going through walk_apply_expr. This ensures
  comments inside array access brackets are properly attached to the index expression.
- Fixed Pmty_functor parameter comments (modType.res now passes): The Rust code was walking
  functor parameters one at a time recursively, which didn't properly handle comments between
  parameters. Changed to match OCaml's approach: collect all parameters first using `functor_type_params`,
  then walk them as a list with `visit_list_but_continue_with_remaining_comments`. Added
  `walk_mod_type_parameter` to walk each parameter's label and optional module type.
- Fixed doc.rs `fits` function performance: The function was cloning the entire stack for each
  `fits` check, causing O(n²) memory allocations with deeply nested structures. Changed to pass
  the first item and rest of stack separately (avoiding clone). Also added iteration limit
  (10000) to prevent exponential blowup with deeply nested custom layouts. This fixes the
  infinite loop/timeout with `nestedCallbacks.res`.
- Fixed unit pattern printing inside braced expressions: `{ () => 1 }` was printing as
  `{ (()) => 1 }`. The special case detection for simple params was using `attrs.is_empty()`
  but the `res.braces` attribute made it non-empty. Fixed by using `filter_parsing_attrs()`.
- Fixed underscore apply sugar printing: `f(a => b, _)` now prints correctly as `f(a => b, _)`
  instead of `__x => f(a => b, __x)`. Added `is_underscore_ident` and `print_underscore_apply`
  functions to detect and print the underscore placeholder pattern.
- Fixed `hard_line` in `line_suffix` processing: The doc.rs line suffix loop wasn't properly
  handling `LineStyle::Hard` in flat mode - it was outputting a space instead of a newline.
- Fixed block expression blank line handling (blockExpr.res now passes)
  - For Pexp_let and Pexp_letexception: extend start_line to leading comment
  - Match OCaml's location extension behavior for blank line calculation
- Fixed Pexp_field comment handling: added print_comments for field name
- Fixed BS object row comments: added cmt_loc wrapper for leading/trailing comments
- Fixed record spread comment placement: /* before */ now appears before ...spread
- Fixed if-else trailing comments: comments between `}` and `else if` cause line break
- Combined consecutive newtype parameters: (type t, type s) -> (type t s)
- Fixed type record spread comments: leading comments like "// spread a" before ...a
- Fixed inline module structure in include statements (include.res): single type alias force_break=false
- Fixed module binding parenthesization and empty structure comments (structure.res)
- Fixed type constraints in with clauses (withConstraints.res): add ptype_cstrs printing
- Fixed empty signature comment handling (signature.res): attach to mod_type loc, not LocIdx::none()
- Fixed functor module type printing: parens around functor in Pmty_with, empty signature formatting
- Fixed binary expression line breaking with `should_inline_rhs_binary_expr` (case.res now passes)
- Fixed binary expression comment attachment (e.g., `a /* c1 */ === /* c2 */ b` preserves comments)
- Fixed case guard block expression comment handling (comments inside `if { ... }` guards)
- Fixed case pattern comment handling (matching OCaml's List.concat [before; inside])
- Fixed doc comment and attribute printing in value bindings (docComments.res)
- Fixed constructor declarations and record field comment handling (typeDefinition.res)
- Fixed recursive module declarations/bindings comment handling (signatureItem.resi)
- Fixed type declarations comment handling with print_listi (structureItem.res)
- Fixed setfield (record field assignment) comment handling
- Fixed record field name comment handling in label declarations
- Fixed module declaration name comment handling
- Fixed attribute payload comments (@@attr(/* c0 */ "here" /* c1 */))
- Fixed exception GADT line breaks with proper constructor argument printing
- Added print_constructor_arguments helper matching OCaml behavior
- Fixed functor parameter comment attachment (leading and trailing)
- Fixed Pmod_apply argument comment handling (using mod_expr_apply)
- Fixed Pmod_constraint location-based comment ordering
- Fixed type extension path comment handling
- Fixed type declaration and external name comment handling
- Fixed exception rebind longident comment handling
- Fixed `@JSX` attribute preservation in braced expressions (braced.res now passes):
  - Removed `"JSX"` from `is_printable_attribute` list - it should be printed, not filtered
  - Fixed function body brace preservation: when extracting inner expr from Pexp_constraint,
    merge attributes from constraint onto inner expression so `@res.braces` is preserved

### Known Issues
- **blockExpr.res**: Fixed! The issue was that OCaml extends row locations to include leading
  comments for Pexp_let and Pexp_letexception. This affects blank line calculation because
  when the extended location is used for comment lookup, the comment isn't found (different key),
  so start_pos falls back to the extended location's start (the comment's line).

- **Binary expression line breaking**: Fixed! Implemented `should_inline_rhs_binary_expr` which
  determines if the RHS of a binary expression can be on a new line. For example, switch
  expressions (Pexp_match) are NOT inlined, so `heightGet(n) >= switch ...` now correctly
  breaks after `>=`. (case.res now passes)

- **expr.res - Newtype parameter comments**: FIXED! The comment_table.rs was computing a span
  from first to last name for NewTypes param_loc, but OCaml uses just the first name's location.
  Changed to use `first.loc` only, which properly attaches leading/trailing comments.

- **expr.res - Callback trailing comments**: FIXED! Implemented special callback printing:
  - `print_pexp_fun` with `in_callback` parameter adds `soft_line` after return expression
  - `print_arguments_with_callback_in_last_position` and `print_arguments_with_callback_in_first_position`
  - Uses `Doc::custom_layout` and `CommentTable::copy` to try different layouts

- **modType.res - Pmty_functor parameter comments**: FIXED! The Rust comment_table was walking
  Pmty_functor parameters one at a time recursively, which didn't properly handle comments
  between parameters. Changed to match OCaml's approach: collect all parameters first using
  `functor_type_params`, then walk them as a list with `visit_list_but_continue_with_remaining_comments`.

- **modType.res - Pmty_typeof**: FIXED! Added partition_by_loc and attach leading/trailing
  to mod_expr.pmod_loc before calling walk_module_expr. Now `module type of /* c4 */ {}`
  keeps the comment before the brace.

- **binaryExpr.res - Flattening**: FIXED! Implemented `flatten_binary_operand` function that
  recursively flattens same-precedence binary operators without creating nested groups/indentation.
  When a binary operand is itself a flattenable binary expression (same precedence, no attributes),
  it recursively flattens instead of wrapping in a new group. This now correctly prints
  `a == b && c == d && e == f` without indenting the middle operators.

### Major Missing Features (blocking remaining ~40 tests)

1. **JSX Printing**: IMPLEMENTED! Basic JSX printing works (fragments, unary elements, container
   elements, props, children). JSX comment handling needs more work - OCaml has sophisticated
   comment partitioning logic (`partition_adjacent_trailing_before_next_token_on_same_line`) that
   determines whether comments attach to tag names, props, or closing tokens.

2. **Callback Formatting**: IMPLEMENTED! Special callback printing that "hugs" callback arguments
   is now working. The basic feature works for expr.res, but callback.res shows some remaining
   layout differences (line breaking decisions for very long callbacks).

3. **Underscore Apply Rewriting**: Need to implement:
   - `rewrite_underscore_apply`: `(__x) => f(a, __x, c)` -> `f(a, _, c)`
   - `rewrite_underscore_apply_in_pipe`: `(__x) => f(__x, a)` -> `f(a)` (omit first _)

4. **Binary Expression Indentation**: Need `should_indent_binary_expr` function that checks:
   - Is it an equality operator?
   - Is the LHS NOT a same-precedence subexpression?
   - Is the operator `:=`?

5. **Inline Record Definitions**: Type declarations with `res.inlineRecordDefinition` attribute
   need special handling - print record fields inline instead of as separate types.

6. **Spread Array Syntax**: Currently prints desugared form `Belt.Array.concatMany([xs, [a, b]])`
   instead of `[...xs, a, b]`.

---

## Phase 1: Comment Handling (Root Cause)

Most printer failures are caused by comment handling issues. Fix these first.

### Core Comment Infrastructure
- [x] **Study OCaml comment attachment** - Read `res_comments_table.ml` thoroughly to understand the algorithm
- [x] **Fix trailing comment preservation** - Comments after expressions/statements get lost
- [x] **Fix blank line preservation around comments** - Blank lines before/after comments should be kept
- [x] **Fix comment placement in function arguments** - `/* c0 */ ~arg=/* c1 */ value /* c2 */` pattern
- [x] **Fix comment placement in function parameters** - Comments on function parameter definitions

### Comment Test Files (1 failing, 16 passing)
- [x] `printer/comments/namedArgs.res` - Named argument comments
- [x] `printer/comments/trailingComments.res` - Trailing comment handling
- [x] `printer/comments/modExpr.res` - Module expression comments
- [x] `printer/comments/structureItem.res` - Structure item comments
- [x] `printer/comments/blockExpr.res` - Block expression comments
- [x] `printer/comments/expr.res` - General expression comments
- [x] `printer/comments/jsx.res` - JSX element comments
- [x] `printer/comments/binaryExpr.res` - Binary expression comments (flattening logic)
- [x] `printer/comments/case.res` - Match case comments
- [x] `printer/comments/array.res` - Array literal comments (spread syntax now implemented)
- [x] `printer/comments/docComments.res` - Doc comment handling
- [x] `printer/comments/typexpr.res` - Type expression comments
- [x] `printer/comments/modType.res` - Module type comments
- [x] `printer/comments/signatureItem.resi` - Signature item comments
- [x] `printer/comments/valueBindingSugar.res` - Value binding sugar comments
- [x] `printer/comments/typeDefinition.res` - Type definition comments
- [x] `printer/comments/extensionConstructor.res` - Extension constructor comments

---

## Phase 2: Expression Printing (13 failing)

### Passing (31 tests)
- [x] `printer/expr/apply.res`
- [x] `printer/expr/array.res`
- [x] `printer/expr/arrayGet.res`
- [x] `printer/expr/arraySet.res`
- [x] `printer/expr/assert.res`
- [x] `printer/expr/block.res`
- [x] `printer/expr/bsObj.res`
- [x] `printer/expr/coerce.res`
- [x] `printer/expr/constant.res`
- [x] `printer/expr/constraint.res`
- [x] `printer/expr/constructor.res`
- [x] `printer/expr/exoticIdent.res`
- [x] `printer/expr/extension.res`
- [x] `printer/expr/field.res`
- [x] `printer/expr/firstClassModule.res`
- [x] `printer/expr/for.res`
- [x] `printer/expr/fun.res`
- [x] `printer/expr/ident.res`
- [x] `printer/expr/if.res`
- [x] `printer/expr/jsObjectAccess.res`
- [x] `printer/expr/jsObjectSet.res`
- [x] `printer/expr/let.res`
- [x] `printer/expr/letexception.res`
- [x] `printer/expr/letmodule.res`
- [x] `printer/expr/letUnwrap.res`
- [x] `printer/expr/nestedCallbacks.res`
- [x] `printer/expr/newtype.res`
- [x] `printer/expr/open.res`
- [x] `printer/expr/pipe.res`
- [x] `printer/expr/record.res`
- [x] `printer/expr/RecordOrObject.res`
- [x] `printer/expr/sequence.res`
- [x] `printer/expr/setfield.res`
- [x] `printer/expr/sideEffects.res`
- [x] `printer/expr/smartPipe.res`
- [x] `printer/expr/ternary.res`
- [x] `printer/expr/try.res`
- [x] `printer/expr/tuple.res`
- [x] `printer/expr/unary.res`
- [x] `printer/expr/Uncurried.res`
- [x] `printer/expr/while.res`
- [x] `printer/expr/whitespace.res`

### Failing (10 tests)
- [x] `printer/expr/asyncAwait.res` - Fixed: attribute positioning in pipe expressions
- [x] `printer/expr/binary.res` - Fixed: exponentiation, single pipe, attributed operands
- [x] `printer/expr/braced.res` - Fixed: @JSX attribute preservation, braces in constrained function bodies
- [ ] `printer/expr/callback.res` - Callback layout differences
- [ ] `printer/expr/dict.res` - Comment handling in dict entries
- [x] `printer/expr/DocComments.res` - Fixed: doc comment inline spacing
- [ ] `printer/expr/jsx.res` - JSX-specific issues
- [x] `printer/expr/list.res` - List spread syntax (fixed with spread list implementation)
- [x] `printer/expr/polyvariant.res` - Fixed: tuple hugging in polyvariant constructors
- [x] `printer/expr/switch.res` - Fixed: trailing comments on same line as `}`
- [ ] `printer/expr/templateLiteral.res` - Template literal printing
- [ ] `printer/expr/UncurriedByDefault.res` - Uncurried by default mode
- [ ] `printer/expr/underscoreApply.res` - Underscore apply rewriting

---

## Phase 3: Module Printing (0 failing - COMPLETE!)

### Module Expressions (all passing)
- [x] `printer/modExpr/apply.res`
- [x] `printer/modExpr/await.res`
- [x] `printer/modExpr/extension.res`
- [x] `printer/modExpr/functor.res`
- [x] `printer/modExpr/include.res`
- [x] `printer/modExpr/structure.res`
- [x] `printer/modExpr/unpack.res`

### Module Types (all passing)
- [x] `printer/modType/exoticIdent.res`
- [x] `printer/modType/extension.res`
- [x] `printer/modType/functor.res`
- [x] `printer/modType/ident.res`
- [x] `printer/modType/moduleTypeOf.res`
- [x] `printer/modType/signature.res`
- [x] `printer/modType/withConstraints.res`

---

## Phase 4: Structure Printing (1 failing)

### Passing (12 tests)
- [x] `printer/structure/attribute.res`
- [x] `printer/structure/exception.res`
- [x] `printer/structure/expr.res`
- [x] `printer/structure/extension.res`
- [x] `printer/structure/external.res`
- [x] `printer/structure/moduleBinding.res`
- [x] `printer/structure/moduleTypeDeclaration.res`
- [x] `printer/structure/open.res`
- [x] `printer/structure/recModules.res`
- [x] `printer/structure/type.res`
- [x] `printer/structure/typeExtension.res`
- [x] `printer/structure/valueBinding.res`

### Failing (1 test)
- [ ] `printer/structure/include.res` - Include with comment formatting

---

## Phase 5: Other Categories

### Signatures (0 failing, 11 passing - COMPLETE!)
- [x] `printer/signature/attributes.resi`
- [x] `printer/signature/exception.resi`
- [x] `printer/signature/extension.resi`
- [x] `printer/signature/include.resi`
- [x] `printer/signature/modtype.resi`
- [x] `printer/signature/module.resi`
- [x] `printer/signature/open.resi`
- [x] `printer/signature/recModule.resi`
- [x] `printer/signature/typext.resi`
- [x] `printer/signature/value.resi`
- [x] `printer/signature/type.resi` - Fixed: inline record definitions

### Patterns (1 failing, 16 passing)
- [x] `printer/pattern/alias.res`
- [x] `printer/pattern/any.res`
- [x] `printer/pattern/array.res`
- [x] `printer/pattern/constant.res`
- [x] `printer/pattern/constraint.res`
- [x] `printer/pattern/construct.res`
- [x] `printer/pattern/exception.res`
- [x] `printer/pattern/exoticIdent.res`
- [x] `printer/pattern/extension.res`
- [x] `printer/pattern/firstClassModules.res`
- [x] `printer/pattern/interval.res`
- [x] `printer/pattern/list.res`
- [x] `printer/pattern/or.res`
- [x] `printer/pattern/record.res`
- [x] `printer/pattern/tuple.res`
- [x] `printer/pattern/type.res`
- [x] `printer/pattern/var.res`
- [x] `printer/pattern/variant.res`
- [ ] `printer/pattern/dict.res` - Dict pattern comments

### Types (1 failing, all typeDef passing)
- [x] `printer/typeDef/abstract.res`
- [x] `printer/typeDef/attributes.res`
- [x] `printer/typeDef/constraint.res`
- [x] `printer/typeDef/exoticIdent.res`
- [x] `printer/typeDef/missingPipeBeforeConstructorAttribute.res`
- [x] `printer/typeDef/open.res`
- [x] `printer/typeDef/private.res`
- [x] `printer/typeDef/record.res`
- [x] `printer/typeDef/typeParams.res`
- [x] `printer/typeDef/variant.res`
- [x] `printer/typexpr/alias.res`
- [x] `printer/typexpr/any.res`
- [x] `printer/typexpr/arrow.res`
- [x] `printer/typexpr/bsObject.res`
- [x] `printer/typexpr/exoticIdent.res`
- [x] `printer/typexpr/extension.res`
- [x] `printer/typexpr/firstClassModule.res`
- [x] `printer/typexpr/objectTypeSpreading.res`
- [x] `printer/typexpr/polyTyp.res`
- [x] `printer/typexpr/tuple.res`
- [x] `printer/typexpr/typeConstr.res`
- [x] `printer/typexpr/var.res`
- [x] `printer/typexpr/variant.res` - Polyvariant comment handling (fixed: wrap tag_doc with print_comments)

### Other (1 failing, 16 passing)
- [x] `printer/other/attributes.res`
- [x] `printer/other/case.res`
- [x] `printer/other/char.res`
- [x] `printer/other/comments.res`
- [x] `printer/other/fatSlider.res`
- [x] `printer/other/home.res`
- [x] `printer/other/lor.res`
- [x] `printer/other/moduleData.res`
- [x] `printer/other/number.res`
- [x] `printer/other/reasonArity.res`
- [x] `printer/other/reasonFile.res`
- [x] `printer/other/reasonInterfaceFile.resi`
- [x] `printer/other/reasonString.res`
- [x] `printer/other/signaturePicker.res`
- [x] `printer/other/StaticReactTypes.res`
- [x] `printer/other/string.res`
- [ ] `printer/other/nesting.res` - Complex nested callback layout

---

## Phase 6: Parser Crashes (Priority!)

Some tests cause the Rust parser to crash (Abort trap: 6). These must be fixed.

- [ ] Investigate and fix parser crashes in infinite loop tests
- [ ] Add proper error handling for malformed input

---

## Phase 7: Non-Printer Tests (CURRENT PRIORITY)

Printer parity is complete (187/187). Now fix the remaining 191 failing tests.

### Test Commands for Each Category

```bash
# Error recovery tests (uses -recover -print ml)
./compiler-rust/target/release/res_parser_rust -recover -print ml tests/syntax_tests/data/parsing/errors/FILE.res

# Grammar tests (uses -print ml without -recover)
./compiler-rust/target/release/res_parser_rust -print ml tests/syntax_tests/data/parsing/grammar/FILE.res

# PPX/React tests (uses -jsx-version 4)
./compiler-rust/target/release/res_parser_rust -jsx-version 4 tests/syntax_tests/data/ppx/react/FILE.res

# AST conversion tests
./compiler-rust/target/release/res_parser_rust -test-ast-conversion -jsx-version 4 tests/syntax_tests/data/ast-mapping/FILE.res

# Compare with OCaml
./_build/install/default/bin/res_parser -recover -print ml tests/syntax_tests/data/parsing/errors/FILE.res
```

### parsing/errors - ~77 failing (error recovery mode)

These tests run with `-recover -print ml` and test error recovery behavior.

**CRITICAL MISSING FEATURE: Consecutive statement/expression error checking**

OCaml has `parse_newline_or_semicolon_expr_block` and `parse_newline_or_semicolon_structure`
functions that check if statements/expressions on the same line are separated by `;` or newline.
If not, they emit "consecutive statements/expressions on a line must be separated by ';' or a newline".

The Rust parser just uses `p.optional(&Token::Semicolon)` and doesn't perform this check.
This is a root cause for MANY of the 77 failing tests - error messages are simply missing.

**Implementation needed:**
1. Add `is_block_expr_start()` function matching OCaml's `Grammar.is_block_expr_start`
2. Add `check_consecutive_expr_block(p)` that:
   - If token is Semicolon -> consume it
   - Else if `is_block_expr_start(token)` AND `p.prev_end_pos.line < p.start_pos.line` -> OK
   - Else if `is_block_expr_start(token)` -> emit "consecutive expressions" error
3. Add `check_consecutive_structure(p)` with similar logic for structure items
4. Replace `p.optional(&Token::Semicolon)` with these functions in parse_block_body() etc.

**OCaml reference:**
- `compiler/syntax/src/res_grammar.ml:266` - `is_block_expr_start` function
- `compiler/syntax/src/res_core.ml:3509` - `parse_newline_or_semicolon_expr_block`
- `compiler/syntax/src/res_core.ml:6335` - `parse_newline_or_semicolon_structure`

**Other issues to fix:**
- [x] Array access syntax (OCaml uses `arr.(i)`, fixed in ML printer)
- [ ] Error message locations - different line:col positions due to different error recovery
- [ ] Recovered AST output - ML printer line breaking differs from OCaml
- [ ] Error hole placement - `[%rescript.exprhole]` vs `()` in recovered AST
- [ ] Error message wording - differs from OCaml's res_diagnostics.ml
- [ ] Path prefix in error locations - `syntax_tests/data/` vs `tests/syntax_tests/data/`

**OCaml reference files:**
- `compiler/syntax/src/res_diagnostics.ml` - Error messages
- `compiler/syntax/src/res_core.ml` - Parser with recovery logic
- `compiler/syntax/src/res_outcome_printer.ml` - ML AST printer

### parsing/grammar - 57 failing

Grammar parsing tests without error recovery.

**ROOT CAUSE: ML printer 80-column line wrapping**

⚠️ **IMPORTANT**: The Rust formatting code (`format.rs`, `doc.rs`, `formatter.rs`) is NOT meant
to be an "emulation" of Format - it must be a **1:1 port** of OCaml's Format module. If there
are differences, look at the OCaml Format source (`stdlib/format.ml`) and make the Rust code
match exactly. Do not try to "fix" or "improve" - just match OCaml's behavior precisely.

Most failures are because OCaml's Format module wraps long lines at 78 columns (not 80!),
but the Rust Formatter doesn't match this behavior exactly. For example:

OCaml output (wraps at 80):
```
include ((sig val s : string val y : int end)[@onSignature ])[@@onInclude
                                                               ]
```

Rust output (doesn't wrap):
```
include ((sig val s : string val y : int end)[@onSignature ])[@@onInclude ]
```

The Rust Formatter implements box-based formatting, but the line-break decisions
don't always match OCaml's Format module. This affects:
- Long type declarations
- Attributes on long lines
- HOV box packing behavior

**Other issues:**
- [ ] Record field punning - `{ a; b }` vs `{ a = a; b = b }`
- [ ] Optional field syntax - `x?` and `name?` printed differently
- [ ] Arrow type attributes - `[@attr]` position differs

### parsing/recovery - 16 failing

Additional recovery mode tests.

### parsing/infiniteLoops - 5 failing

Tests that previously caused infinite loops. May involve parser crashes.

### parsing/other - 3 failing

Miscellaneous parsing tests.

### ppx/react - 0 failing ✅

JSX transformation tests using `-jsx-version 4`. All passing!

### conversion - 0 failing ✅

ML/Reason to ReScript conversion tests. All passing!

### ast-mapping - 0 failing ✅

AST conversion tests using `-test-ast-conversion`. All passing!

---

## Commands Reference

```bash
# Run all syntax tests (MUST PASS ALL for completion)
PARSER=rust ./scripts/test_syntax.sh 2>&1 | tail -50

# Build after changes
cargo build --manifest-path compiler-rust/Cargo.toml --release

# Test single file - printer mode (default)
./compiler-rust/target/release/res_parser_rust FILE.res

# Test single file - error recovery mode
./compiler-rust/target/release/res_parser_rust -recover -print ml FILE.res

# Test single file - grammar mode (no recovery)
./compiler-rust/target/release/res_parser_rust -print ml FILE.res

# Test single file - JSX PPX mode
./compiler-rust/target/release/res_parser_rust -jsx-version 4 FILE.res

# Test single file - AST conversion mode
./compiler-rust/target/release/res_parser_rust -test-ast-conversion -jsx-version 4 FILE.res

# Compare with OCaml (printer mode)
./_build/install/default/bin/res_parser FILE.res

# Compare with OCaml (error recovery mode)
./_build/install/default/bin/res_parser -recover -print ml FILE.res

# View diff for failing test
diff tests/syntax_tests/data/PATH/expected/FILE.txt \
     tests/temp/syntax_tests/data/PATH/expected/FILE.txt

# List all failing tests
grep '^FAIL:' tests/temp/results.txt | cut -d: -f2
```

---

## Notes

- Focus on **root causes** not symptoms - many failures share the same underlying issue
- Printer tests (Phase 1-6) are COMPLETE - focus on Phase 7 (non-printer tests)
- Commit after EVERY improvement, even if just 1 more test passes
- Study the OCaml code, don't guess!
- **If a task is hard, don't skip it to find an easier one** - commit your progress and keep working on it
- **The task is NOT complete until ALL 506 syntax tests pass**
- Output `RALPH_COMPLETE` only when `PARSER=rust ./scripts/test_syntax.sh` shows 0 failures
