# Printing Parity TODO

**Last Updated:** 2026-02-01
**Overall Status:** 254/506 tests passing (50%)
**Printer Status:** 135/187 tests passing (72%)

### Recent Progress
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
- **blockExpr.res**: 6 remaining differences. After deep investigation, the blank line issue
  appears to be related to how OCaml's document rendering works, not the document generation.
  Both Rust and OCaml generate similar Doc structures, but the rendering differs for certain
  cases. The specific pattern is: after `open X` statement when followed by a single-line
  comment, OCaml sometimes adds a blank line before the comment. This happens when the
  expression body is an identifier (like `foo`) but NOT when it's a function call (like `foo()`).
  This might be due to document `break_parent` propagation or group breaking logic.

- **Binary expression line breaking**: Fixed! Implemented `should_inline_rhs_binary_expr` which
  determines if the RHS of a binary expression can be on a new line. For example, switch
  expressions (Pexp_match) are NOT inlined, so `heightGet(n) >= switch ...` now correctly
  breaks after `>=`. (case.res now passes)

---

## Phase 1: Comment Handling (Root Cause)

Most printer failures are caused by comment handling issues. Fix these first.

### Core Comment Infrastructure
- [x] **Study OCaml comment attachment** - Read `res_comments_table.ml` thoroughly to understand the algorithm
- [x] **Fix trailing comment preservation** - Comments after expressions/statements get lost
- [x] **Fix blank line preservation around comments** - Blank lines before/after comments should be kept
- [x] **Fix comment placement in function arguments** - `/* c0 */ ~arg=/* c1 */ value /* c2 */` pattern
- [x] **Fix comment placement in function parameters** - Comments on function parameter definitions

### Comment Test Files (15 failing)
- [x] `printer/comments/namedArgs.res` - Named argument comments
- [x] `printer/comments/trailingComments.res` - Trailing comment handling
- [x] `printer/comments/modExpr.res` - Module expression comments
- [x] `printer/comments/structureItem.res` - Structure item comments
- [ ] `printer/comments/blockExpr.res` - Block expression comments
- [ ] `printer/comments/expr.res` - General expression comments
- [ ] `printer/comments/jsx.res` - JSX element comments
- [ ] `printer/comments/binaryExpr.res` - Binary expression comments
- [x] `printer/comments/case.res` - Match case comments
- [ ] `printer/comments/array.res` - Array literal comments
- [x] `printer/comments/docComments.res` - Doc comment handling
- [ ] `printer/comments/typexpr.res` - Type expression comments
- [ ] `printer/comments/modType.res` - Module type comments
- [x] `printer/comments/signatureItem.resi` - Signature item comments
- [ ] `printer/comments/valueBindingSugar.res` - Value binding sugar comments
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
