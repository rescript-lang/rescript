# Printing Parity TODO

**Last Updated:** 2026-02-02
**Overall Status:** 292/506 tests passing (57%)
**Printer Status:** 168/187 tests passing (89%)

### Recent Progress
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
- [ ] `printer/comments/array.res` - Array literal comments (needs spread syntax)
- [x] `printer/comments/docComments.res` - Doc comment handling
- [x] `printer/comments/typexpr.res` - Type expression comments
- [x] `printer/comments/modType.res` - Module type comments
- [x] `printer/comments/signatureItem.resi` - Signature item comments
- [x] `printer/comments/valueBindingSugar.res` - Value binding sugar comments
- [x] `printer/comments/typeDefinition.res` - Type definition comments
- [x] `printer/comments/extensionConstructor.res` - Extension constructor comments

---

## Phase 2: Expression Printing (31 failing)

Many of these may be fixed by Phase 1 comment fixes.

### JSX-Related
- [ ] `printer/expr/jsx.res` - JSX expression printing
- [ ] `printer/expr/switch.res` - JSX inside switch arms

### Core Expressions
- [ ] `printer/expr/block.res` - Block expression formatting
- [ ] `printer/expr/callback.res` - Callback function formatting
- [ ] `printer/expr/nestedCallbacks.res` - Nested callback formatting
- [ ] `printer/expr/fun.res` - Function expression formatting
- [ ] `printer/expr/let.res` - Let expression formatting
- [ ] `printer/expr/try.res` - Try expression formatting
- [ ] `printer/expr/ternary.res` - Ternary expression formatting

### Operators & Access
- [ ] `printer/expr/binary.res` - Binary operator formatting
- [ ] `printer/expr/unary.res` - Unary operator formatting
- [ ] `printer/expr/arrayGet.res` - Array access formatting
- [ ] `printer/expr/arraySet.res` - Array assignment formatting
- [ ] `printer/expr/jsObjectAccess.res` - JS object access
- [ ] `printer/expr/jsObjectSet.res` - JS object assignment
- [ ] `printer/expr/setfield.res` - Record field assignment

### Data Structures
- [ ] `printer/expr/record.res` - Record expression formatting
- [ ] `printer/expr/list.res` - List literal formatting
- [ ] `printer/expr/dict.res` - Dict literal formatting
- [ ] `printer/expr/polyvariant.res` - Polyvariant formatting

### Special Cases
- [ ] `printer/expr/asyncAwait.res` - Async/await formatting
- [ ] `printer/expr/templateLiteral.res` - Template literal formatting
- [ ] `printer/expr/smartPipe.res` - Pipe operator formatting
- [ ] `printer/expr/newtype.res` - Newtype formatting
- [ ] `printer/expr/exoticIdent.res` - Exotic identifier formatting
- [ ] `printer/expr/braced.res` - Braced expression formatting
- [ ] `printer/expr/bsObj.res` - BS object formatting
- [ ] `printer/expr/underscoreApply.res` - Underscore apply formatting
- [ ] `printer/expr/Uncurried.res` - Uncurried function formatting
- [ ] `printer/expr/UncurriedByDefault.res` - Uncurried by default formatting
- [ ] `printer/expr/DocComments.res` - Doc comments in expressions

---

## Phase 3: Module Printing (6 failing)

### Module Expressions (3 failing)
- [ ] `printer/modExpr/functor.res` - Functor expression printing
- [ ] `printer/modExpr/include.res` - Include expression printing
- [ ] `printer/modExpr/structure.res` - Module structure printing

### Module Types (0 failing - COMPLETE!)
- [x] `printer/modType/functor.res` - Functor type printing
- [x] `printer/modType/signature.res` - Signature printing
- [x] `printer/modType/withConstraints.res` - With constraints printing

---

## Phase 4: Structure Printing (7 failing)

- [ ] `printer/structure/valueBinding.res` - Value binding formatting
- [ ] `printer/structure/type.res` - Type definition formatting
- [ ] `printer/structure/moduleBinding.res` - Module binding formatting
- [ ] `printer/structure/recModules.res` - Recursive modules formatting
- [ ] `printer/structure/exception.res` - Exception definition formatting
- [ ] `printer/structure/include.res` - Include statement formatting
- [ ] `printer/structure/typeExtension.res` - Type extension formatting

---

## Phase 5: Other Categories

### Signatures (2 failing)
- [ ] `printer/signature/type.resi` - Type signature formatting
- [ ] `printer/signature/recModule.resi` - Recursive module signatures

### Patterns (2 failing)
- [ ] `printer/pattern/constant.res` - Constant pattern formatting
- [ ] `printer/pattern/dict.res` - Dict pattern formatting

### Types (2 failing)
- [ ] `printer/typeDef/variant.res` - Variant type definition
- [ ] `printer/typexpr/variant.res` - Variant type expression

### Other (6 failing)
- [ ] `printer/other/attributes.res` - Attribute formatting
- [ ] `printer/other/nesting.res` - Deep nesting formatting
- [ ] `printer/other/home.res` - Real-world example
- [ ] `printer/other/signaturePicker.res` - Real-world example
- [ ] `printer/other/fatSlider.res` - Real-world example
- [ ] `printer/other/StaticReactTypes.res` - Real-world example

---

## Phase 6: Parser Crashes (Priority!)

Some tests cause the Rust parser to crash (Abort trap: 6). These must be fixed.

- [ ] Investigate and fix parser crashes in infinite loop tests
- [ ] Add proper error handling for malformed input

---

## Phase 7: Non-Printer Tests

After printer parity is achieved, address remaining categories:

### Parsing Tests (~155 failing)
- [ ] `parsing/errors` - 78 failing (error recovery)
- [ ] `parsing/grammar` - 77 failing (grammar coverage)

### Other Categories (~30 failing)
- [ ] `conversion` - 12 failing
- [ ] `ppx/react` - 11 failing
- [ ] `parsing/recovery` - 16 failing
- [ ] `parsing/infiniteLoops` - 5 failing

---

## Commands Reference

```bash
# Run all syntax tests
PARSER=rust ./scripts/test_syntax.sh 2>&1 | tail -50

# Build after changes
cargo build --manifest-path compiler-rust/Cargo.toml --release

# Test single file
./compiler-rust/target/release/res_parser_rust FILE.res

# Compare with OCaml
./_build/install/default/bin/res_parser FILE.res

# View diff for failing test
diff tests/syntax_tests/data/PATH/expected/FILE.txt \
     tests/temp/syntax_tests/data/PATH/expected/FILE.txt
```

---

## Notes

- Focus on **root causes** not symptoms - many failures share the same underlying issue
- Comment handling is the biggest blocker - fix Phase 1 first
- Commit after EVERY improvement, even if just 1 more test passes
- Study the OCaml code, don't guess!
- **If a task is hard, don't skip it to find an easier one** - commit your progress and keep working on it. Everything needs to be done eventually.
