# Design: Early Return in Functions

## Context & Motivation
Developers regularly ask for a lightweight way to exit a function before its final expression. Today they must emulate early exits using nested conditionals, exceptions, or helper functions, which obscures intent and bloats JavaScript output. Supporting a first-class `return` keyword improves readability, enables more idiomatic interop with JavaScript, and narrows the ergonomics gap with other languages while preserving ReScript's expression-oriented style.

## Goals
- Introduce a `return` expression that exits the innermost function, optionally carrying a value (`return expr` or `return;`).
- Type-check `return` so that subsequent code is treated as unreachable, avoiding spurious exhaustiveness warnings.
- Emit direct JavaScript `return` statements to make async and `try` interactions behave exactly like plain JS.
- Preserve backward compatibility for existing code that does not use `return`.

## Non-goals
- Adding multi-value returns or early exit for non-function constructs (loops, switches without functions, etc.).
- Introducing new runtime constructs beyond the emitted JavaScript `return`.
- Changing module-level or top-level behaviour; `return` remains illegal outside function bodies.

## Semantics Overview
- `return` is an expression with the bottom-like type `never`. The payload, when present, must unify with the enclosing function's declared result.
- `return` targets only the innermost function scope, including anonymous functions and closures.
- `return;` is syntactic sugar for `return ();` but keeps type `never` so the function's result type must be `unit`.
- Once a `return` is evaluated, control flow stops at that point; subsequent expressions in the same block are unreachable.

## Syntax Layer Changes (`compiler/syntax/`)
- Extend the grammar in `parser.mly` to parse `return` as a `simple_expr` with an optional trailing expression (`return`, `return expr`).
- Add `Pexp_return of expression option` to `parsetree.ml`, and update related helpers (`ast_iterator`, printers, etc.).
- Mirror the changes in `ast_mapper_from0.ml` and `ast_mapper_to0.ml` to maintain compatibility with `parsetree0.ml` (which must stay frozen).
- Update syntax error recovery to produce messages such as “`return` is only allowed inside function bodies" when seen in invalid positions.
- Add new parser fixtures under `tests/syntax_tests/` (positive and negative cases).

## Typed Tree & Type Checking (`compiler/ml/`)
- Introduce `Texp_return` in `Typedtree.expression_desc` and a corresponding record in `Typedtree_helper` utilities.
- Extend `typecore.ml` to:
  - Ensure we are inside a function context (reusing or extending `env.in_function`).
  - Type-check the optional payload against the enclosing function's result type.
  - Assign the new `never` type to the expression so downstream phases treat it as non-returning.
  - Register an error when used outside functions or when the payload type mismatches.
- Introduce a dedicated bottom type constructor (`never`) if one does not already exist:
  - Extend `Types.type_desc` and helpers in `btype.ml` / `ctype.ml` with `Tnever` (or similar) plus `Predef` registration, including printer support in `printtyp.ml`.
  - Update utility predicates (`Types.maybe_bottom`, dead-code checks) to understand the new type.
- Ensure exhaustiveness and dead code analysis (e.g. `parmatch.ml`, `clflags.warn_error`) treat `never` as non-fallthrough so we avoid double warnings.
- Update typed tree iterators and printers (`TypedtreeIter`, `Printtyped`) to handle `Texp_return`.

## Lambda IR Translation (`compiler/core/`)
- Extend `lam.ml` with an `Lreturn of lambda option` constructor (or reuse existing exit nodes if we can adapt them).
- Modify `translcore.ml` (and related helpers) to translate `Texp_return` into the new lambda form, marking the generated continuation as finished.
- Adjust passes that manipulate control flow:
  - Ensure `lam_pass_exits`, `lam_dce`, and similar optimizations treat `Lreturn` as terminating.
  - Update `lam_print.ml` and analysis utilities to print and traverse the new node.

## JavaScript Backend (`compiler/core/js_*`)
- Update JS lowering (`lam_compile.ml`, `js_output.ml`) so lambda outputs marked as “finished” get converted to `return_stmt payload` and no additional implicit return is appended.
- Ensure `switch`/`if` lowering avoids emitting duplicate `return` statements when a branch already ends with `return`. This likely relies on `output_finished = True` plumbing already used by `throw` and existing returns.
- Adjust `js_stmt_make` / `js_exp_make` to expose helper constructors where needed, and audit passes like `js_pass_flatten.ml` to respect terminating statements.
- Validate async helpers and promise sugar to confirm the generated functions contain direct `return` statements, ensuring semantics match JavaScript.

## Tooling & Diagnostics
- Update AST printers (`pprintast.ml`, `js_dump_*`) to display `return` expressions.
- Extend the language server (`analysis/`) to surface the new node in hover/type info and to provide quick-fix diagnostics.
- Document the feature in `docs/Syntax.md`, including examples and restrictions.

## Migration & Compatibility
- Existing code continues to compile; no change to default behaviour.
- PPX compatibility: because `parsetree0.ml` remains frozen, PPXs continue to receive the v0 AST without `return`. We maintain compatibility by mapping `Pexp_return` to/from the v0 representation through `ast_mapper_from0` / `ast_mapper_to0`.
- JavaScript output remains stable aside from functions that now contain explicit `return` statements when developers opt in to the new feature.

## Testing Strategy
- **Syntax tests**: new fixtures for valid/invalid `return` usages, nested functions, and top-level errors.
- **Typechecker tests** (`tests/ounit_tests/` or similar): ensure payload type mismatches raise errors, unreachable code warnings are produced, and nested function scoping works.
- **Lambda / JS IR tests**: add golden-print tests verifying `Lreturn` in `lam_print` and generated JS blocks for representative cases (`if`, `switch`, `try/finally`, async wrappers).
- **Integration tests** (`tests/build_tests/`): demonstrate runtime behaviour, including interaction with promise helpers and exceptions.

## Open Questions & Follow-ups
- Does the compiler already expose a notion of bottom in other phases? If so, integrate rather than re-invent; otherwise, ensure `never` is threaded consistently (e.g. into `Predef` and external tooling).
- Should `return` be allowed inside `fun { } =>` pipelines or only as a standalone statement expression? Current proposal allows it anywhere expressions are permitted, but we should validate editor ergonomics and readability.
- Consider introducing linting guidance to discourage overuse in expression-heavy code while still allowing pragmatic escapes.

