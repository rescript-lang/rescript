# Parser Error Reporting Progress

## Current Status: Phase 3 Complete - typeParams.res Passing

## Test Results

### Current Status
```
typeParams.res error test: PASSING ✓
Other error tests: Need specialized error detection
Roundtrip tests: ~50% AST parity (formatting differences remain)
```

## Phase 1: Error Formatter ✓ COMPLETE

### Tasks
- [x] Create `code_frame.rs` module with source context printing
- [x] Implement `seek_lines_before` - find start of context window
- [x] Implement `seek_lines_after` - find end of context window
- [x] Implement line number formatting with padding
- [x] Implement leading whitespace stripping
- [x] Implement line elision for large ranges (>5 lines)
- [x] Add "Syntax error!" header formatting
- [x] Format file path with line:column range
- [x] Integrate into CLI to output before ML output
- [x] Extra blank line after error code frame and message

### Status
Error formatting is fully working and matches OCaml output format.

## Phase 2: ML Printer Formatting ✓ COMPLETE

### Tasks
- [x] Add newlines between structure items
- [x] Fix record field indentation (2-space indent)
- [x] Fix record type formatting (break before { for multi-param types)
- [x] Fix arrow arity annotation `(a:n)`
- [x] Remove trailing newline to match OCaml exactly

### Status
ML printer now matches OCaml for the typeParams.res test case.

## Phase 3: Error Recovery ✓ COMPLETE (for typeParams.res)

### Completed Fixes

1. **Specialized error detection for type params with old-style parens**:
   - Added `parse_type_params_old_style()` function
   - Detects `type foo('a)` and suggests `type foo<'a>`
   - Added `format_type_with_angle_brackets()` helper

2. **Reserved keyword detection in type params**:
   - Detects `'for`, `'let`, etc. and reports helpful error
   - Message: "for is a reserved keyword. Keywords need to be escaped: \"for\""

3. **Invalid type param pattern detection**:
   - Detects `'+`, `'_` and reports error
   - Detects missing singlequote (`foo` instead of `'foo`)

4. **Multiple error reporting**:
   - Added `err_multiple()` to parser state
   - Allows reporting multiple errors without silencing

5. **Error position accuracy**:
   - Fixed to report position at the invalid token, not the preceding singlequote

6. **Recovery produces valid AST**:
   - Uses `Ptyp_var(String::new())` for invalid type params (prints as `'`)
   - Continues parsing to find all errors

### Test Status
```
typeParams.res: PASSING ✓
```

## Completed Items

1. **Error formatter** (`code_frame.rs`)
   - Source context with 2 lines before/after
   - Line numbers with proper padding
   - "Syntax error!" header
   - File path with line:column
   - Extra blank lines matching OCaml format
   - Helpful error messages via `explain()`

2. **CLI integration**
   - Errors output to stdout (not stderr)
   - Errors printed before ML output in recover mode
   - Proper exit code handling

3. **ML printer improvements**
   - Newlines between structure items
   - Record field indentation (2 spaces)
   - Record type formatting with line break for multi-param types
   - Arrow arity annotation `(a:n)`
   - No trailing newline

4. **Error detection for type params**
   - Old-style parens `type foo('a)` detected with suggestion
   - Reserved keywords in type params detected
   - Invalid patterns like `'+`, `'_` detected
   - Missing singlequote detected
   - Multiple errors reported

5. **Error recovery**
   - Produces valid AST for type definitions with errors
   - Uses empty type var for invalid type params

## Remaining Work

1. **Other error tests** - Need specialized error detection for each test case:
   - record.res, arrow.res, etc.
   - Each has different error patterns needing custom handling

2. **AST parity** - Sexp output differs from OCaml in some cases:
   - List literals (`list{...}` uses extension vs `::` constructor)
   - Function call with unit (`f()` argument handling)
   - Pipe operator `->` handling
   - String escaping in identifiers

## Test Commands

```bash
# Run a single error test
cd tests && ../compiler-rust/target/release/res_parser_rust -r -p ml syntax_tests/data/parsing/errors/typeDef/typeParams.res

# Compare with OCaml
cd tests && ../_build/install/default/bin/res_parser -recover -print ml syntax_tests/data/parsing/errors/typeDef/typeParams.res

# Test AST parity
./scripts/test_parser_ast_parity.sh
```
