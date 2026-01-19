# Task: Parser Error Reporting and Recovery

## Overview

The Rust parser needs to match the OCaml parser's error reporting format for the syntax test suite to pass. This includes:

1. **Formatted error messages** with source context
2. **Quality error recovery** that produces valid ASTs
3. **ML printer formatting** with proper indentation

## Current State

The Rust parser has basic diagnostics but outputs them differently than OCaml:

**OCaml output** (expected):
```
  Syntax error!
  syntax_tests/data/parsing/errors/typeDef/typeParams.res:1:10-13

  1 │ type node('a) = {
  2 │   _value: Js.Nullable.value<'a>
  3 │ }

  Type parameters require angle brackets:
  node<'a>

type nonrec 'a node = {
  _value: 'a Js.Nullable.value }
```

**Rust output** (current):
```
type nonrec node ;;[%%rescript.exprhole] ;;a ;;{_value = ...
```

Issues:
- No formatted error messages with source context
- Poor error recovery (many `[%%rescript.exprhole]` placeholders)
- ML output all on one line, not formatted

## Requirements

### 1. Error Message Formatter

Create a formatter that outputs errors matching OCaml's format:

```
  Syntax error!
  <filename>:<line>:<col>[-<end_col>]

   <N-2> │ <context line before>
   <N-1> │ <context line before>
   <N>   │ <error line - highlighted portion>
   <N+1> │ <context line after>
   <N+2> │ <context line after>

  <helpful_message>
```

Features needed (based on `compiler/ml/code_frame.ml`):
- [ ] "Syntax error!" header (2 spaces indent)
- [ ] File path with line:column range (2 spaces indent)
- [ ] Source context: 2 lines before and 2 lines after error
- [ ] Line numbers right-aligned with proper padding
- [ ] Vertical bar separator (│ or ┆ if indentation stripped)
- [ ] Strip common leading whitespace from lines
- [ ] Elide middle lines if error spans > 5 lines (show "...")
- [ ] Color support (red for errors, but works without colors too)
- [ ] Helpful error explanations (already have `explain()` method)

Reference implementation: `compiler/ml/code_frame.ml` (284 lines)

### 2. Error Recovery Improvements

The parser needs smarter recovery strategies:

Current problems:
- Creates too many placeholder nodes (`[%%rescript.exprhole]`)
- Doesn't skip to synchronization points properly
- Cascading errors from single mistakes

Needed improvements:
- [ ] Better synchronization point detection (`;`, `}`, `let`, `type`, etc.)
- [ ] Skip invalid tokens until valid continuation found
- [ ] Recover type definitions with wrong syntax (e.g., `type node('a)` → `type node<'a>`)
- [ ] Don't create placeholder nodes when skipping
- [ ] Limit error cascading

### 3. ML Printer Formatting

The ML output printer needs proper formatting:

Current: All on one line
```
type nonrec node ;;[%%rescript.exprhole] ;;a ...
```

Expected: Proper indentation and newlines
```ocaml
type nonrec 'a node = {
  _value: 'a Js.Nullable.value }
```

Needed:
- [ ] Newlines between structure items
- [ ] Proper indentation for record fields
- [ ] Correct OCaml syntax for all constructs
- [ ] Handle recovered/placeholder nodes gracefully

## Test Files

The tests are in `tests/syntax_tests/data/parsing/`:
- `errors/` - Files with intentional syntax errors
- `recovery/` - Files testing error recovery
- `infiniteLoops/` - Files that could cause parser hangs
- `grammar/` - Valid grammar tests
- `other/` - Other parsing tests

Each test file has an expected output in `expected/<filename>.txt`.

The test runs:
```bash
# For error/recovery tests
res_parser -recover -print ml <file>

# For grammar tests
res_parser -print ml <file>
```

## Implementation Plan

### Phase 1: Error Formatter
1. Create `ErrorFormatter` struct in `diagnostics.rs`
2. Add method to format error with source context
3. Integrate into CLI to output errors before ML output

### Phase 2: ML Printer
1. Fix indentation in `print_structure_ml` and related functions
2. Add newlines between items
3. Handle edge cases (empty structures, nested modules)

### Phase 3: Error Recovery
1. Audit current recovery points in parser
2. Add synchronization token detection
3. Improve recovery for common error patterns
4. Test with error test suite

## Success Criteria

- All tests in `tests/syntax_tests/data/parsing/` produce output matching expected files
- `./scripts/test_syntax.sh` passes without diffs
- Error messages are helpful and match OCaml format
- Recovery produces reasonable ASTs (not full of placeholders)

## References

- OCaml parser: `compiler/syntax/src/res_diagnostics.ml`
- OCaml printer: `compiler/syntax/src/res_printer.ml`
- Test script: `scripts/test_syntax.sh`
