# Printing Parity TODO

**Last Updated:** 2026-02-01
**Overall Status:** 236/506 tests passing (47%)
**Printer Status:** 119/187 tests passing (64%)

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
- [ ] `printer/comments/structureItem.res` - Structure item comments
- [ ] `printer/comments/blockExpr.res` - Block expression comments
- [ ] `printer/comments/expr.res` - General expression comments
- [ ] `printer/comments/jsx.res` - JSX element comments
- [ ] `printer/comments/binaryExpr.res` - Binary expression comments
- [ ] `printer/comments/case.res` - Match case comments
- [ ] `printer/comments/array.res` - Array literal comments
- [ ] `printer/comments/docComments.res` - Doc comment handling
- [ ] `printer/comments/typexpr.res` - Type expression comments
- [ ] `printer/comments/modType.res` - Module type comments
- [ ] `printer/comments/signatureItem.resi` - Signature item comments
- [ ] `printer/comments/valueBindingSugar.res` - Value binding sugar comments
- [ ] `printer/comments/typeDefinition.res` - Type definition comments
- [ ] `printer/comments/extensionConstructor.res` - Extension constructor comments

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

### Module Types (3 failing)
- [ ] `printer/modType/functor.res` - Functor type printing
- [ ] `printer/modType/signature.res` - Signature printing
- [ ] `printer/modType/withConstraints.res` - With constraints printing

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
