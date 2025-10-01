# Design: Early Return in Functions

## Context & Motivation
Developers regularly ask for a lightweight way to exit a function before its final expression. Today they must emulate early exits using nested conditionals, exceptions, or helper functions, which obscures intent and bloats JavaScript output. Supporting a first-class `return` keyword improves readability, enables more idiomatic interop with JavaScript, and narrows the ergonomics gap with other languages while preserving ReScript's expression-oriented style.

## Goals
- Introduce a `return` expression that exits the innermost function, optionally carrying a value (`return expr` or bare `return`).
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
- A bare `return` is sugar for returning `unit`, still typed as `never`.
- Once a `return` is evaluated, control flow stops at that point; subsequent expressions in the same block are unreachable.

## Syntax Layer Changes (`compiler/syntax/`)
- Extend the grammar handled in `compiler/syntax/src/res_parser.ml` (and related helpers such as `res_grammar.ml`) to parse `return` as an expression with an optional trailing payload (`return` or `return expr`).
- Add `Pexp_return of expression option` to `parsetree.ml`, and update related helpers (`ast_iterator`, printers, etc.).
- Mirror the changes in `ast_mapper_from0.ml` and `ast_mapper_to0.ml` to maintain compatibility with `parsetree0.ml` (which must stay frozen).
- Update syntax error recovery to produce messages such as “`return` is only allowed inside function bodies" when seen in invalid positions.
- Add new parser fixtures under `tests/syntax_tests/` (positive and negative cases).

## Typed Tree & Type Checking (`compiler/ml/`)
- Introduce `Texp_return` in `Typedtree.expression_desc` (update `compiler/ml/typedtree.mli` and `typedtree.ml`) and thread it through the existing iterators/printers.
- Extend `typecore.ml` to:
  - Reject uses outside functions by reusing the existing optional `in_function` plumbing that `type_function` already threads through.
  - Type-check the optional payload against the enclosing function's result type.
- Populate the new node with a freshly created type variable (mirroring how `%raise` is typed today) so downstream phases treat it as non-returning without introducing a bespoke primitive type.
  - Emit appropriate errors on context or payload mismatches.
  - Keep `type_statement` warning behaviour intact so `return` inherits the existing `Warnings.Nonreturning_statement` flow (`compiler/ml/typecore.ml:3884-3894`).
- If the type-variable approach proves insufficient, adding an explicit bottom constructor would require touching `Types.type_desc` plus `btype.ml`, `ctype.ml`, `predef.ml`, and the printers in `printtyp.ml`, but the current pipeline already models non-returning code via `Tvar`.
- Ensure exhaustiveness and dead code analysis (e.g. `compiler/ml/parmatch.ml`, `compiler/ext/warnings.ml`) treat `return` as non-fallthrough so we avoid double warnings.
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
- Adjust `js_stmt_make` / `js_exp_make` to expose helper constructors where needed, and audit passes like `js_pass_flatten_and_mark_dead.ml` to respect terminating statements.
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

## Existing Unreachable Code Handling
- **Typechecker warnings**: `type_statement` warns with `Warnings.Nonreturning_statement` whenever an expression typed as a bare `Tvar` is discarded (`compiler/ml/typecore.ml:3884-3894`), which is how `%raise` communicates non-returning behaviour today.
- **Pattern reachability**: `Parmatch.check_unused` emits `Warnings.Unreachable_case` for dead match arms and already runs for every `Texp_match`/`Texp_function` (`compiler/ml/parmatch.ml:2158-2201`).
- **Backend pruning**: `%raise` lowers to `Lprim (Praise, …)` in `translcore` (`compiler/ml/translcore.ml:738-745`). The JS backend recognises that primitive and marks the output as finished (`compiler/core/lam_compile.ml:1540-1560`), and `Js_output.append_output` drops any subsequent statements when `output_finished = True` (`compiler/core/js_output.ml:82-138`). A future `return` node should reuse this plumbing so dead statements are automatically discarded without a new bottom type.

## Open Questions & Follow-ups
- The compiler already models non-returning expressions via fresh type variables plus warning logic (`compiler/ml/typecore.ml:3884-3894`) and by marking backend outputs as finished (`compiler/core/lam_compile.ml:1540-1560`, `compiler/core/js_output.ml:117-138`). Reuse that machinery for `return` before introducing a dedicated `never` constructor.
- Validate how `return` reads inside pipeline-heavy expressions; current proposal allows it everywhere, but we should document guidance if certain patterns feel awkward.
- Consider introducing linting guidance to discourage overuse in expression-heavy code while still allowing pragmatic escapes.
