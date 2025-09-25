# Migration Framework – Current Capabilities

This document captures what the migration framework currently supports, based on `tools/src/migrate.ml` (and helpers in `tools/src/transforms.ml`, `compiler/ml/builtin_attributes.ml`, and `analysis/src/Cmt.ml`).

## Inputs & Preconditions

- Targets: `*.res` and `*.resi` files.
- Requires build artifacts for the file’s module (CMT/CMTI). If not found, migration aborts with an error message asking to build the project.
- Output modes: write back to file or print to stdout.

## Deprecation Sources

- Migration data is harvested from `@deprecated({ reason, migrate, migrateInPipeChain })` attributes recorded in CMT extras.
- Captured context where deprecation was used:
  - `FunctionCall` – a function call site
  - `Reference` – a value/identifier reference (non‑call)

## Supported Rewrites

- Direct function calls (non‑pipe):
  - If `migrate` is an application expression, the call is rewritten according to the template (see Templates & Placeholders).
- Pipe calls using `->`:
  - If `migrateInPipeChain` is provided, it is used for piped form. Special handling for single‑step chains (see Pipe Semantics).
- Identifier references (non‑call):
  - If `migrate` is provided, the reference is replaced by that expression.
  - Special case: if the template is `f()` (unit call), treat it as `f` to avoid adding a spurious unit application.
- Type constructor references:
  - If `migrate` is `%replace.type(: <core_type>)`, type constructor occurrences within the deprecation’s location range are replaced by `<core_type>`.
  - If the template is a constructor with its own type arguments, original type arguments are appended; otherwise original arguments are dropped.
- Extension rename:
  - Extension `todo_` is remapped to `todo`.

## Templates & Placeholders

- Template form: application expressions only for call‑site rewrites; arbitrary expressions for reference rewrites.
- Placeholders inside template expressions:
  - `%insert.unlabelledArgument(<int>)`
    - 0‑based index into the source call’s unlabelled arguments.
  - `%insert.labelledArgument("<name>")`
    - Refers to a labelled or optional source argument by name.
- Placeholder replacement occurs anywhere inside the template expression, not only as direct arguments.
- Consumed arguments are dropped from the original call; remaining arguments keep order.
- Label renaming:
  - If a template argument has a label and its expression is a labelled placeholder, the corresponding source argument is emitted under the template’s label (rename).

## Pipe Semantics

- For piped calls, the pipe LHS is considered unlabelled argument index 0 for placeholder resolution.
- When constructing the inner call, unlabelled drop positions are adjusted to account for the fact that the LHS does not appear as an inner argument.
- Single‑step collapse:
  - If the LHS is not itself a pipe and `migrateInPipeChain` exists:
    - If `migrate` exists, prefer collapsing the step by applying `migrate` with the LHS inserted as unlabelled argument index 0.
    - Otherwise use `migrateInPipeChain` in piped form.
- Empty‑args piped form:
  - If piping into a bare identifier and the chosen piped template inserts no arguments, the result remains `lhs -> newFn` (no extra parentheses).

## Transforms (`@apply.transforms`)

- Attribute name: `@apply.transforms(["<id>", ...])` on expressions.
- Resolution: IDs map to functions in `tools/src/transforms.ml`. Unknown IDs are ignored.
- Application:
  - Attributes attached to template or placeholder expressions are preserved on replacements and applied in a second pass over `.res` implementations.
  - Currently implemented transform: `dropUnitArgumentsInApply` (drops only unlabelled unit arguments from application nodes).
  - Other registry entries are stubs and currently no‑ops.
- Note: The second pass runs for `.res` (implementations). Interfaces (`.resi`) do not contain expression bodies; transform pass is not applied there.

## Limitations / Not Supported (Today)

- Call‑site rewrite requires the template to be an application expression; non‑application templates for calls are ignored.
- Placeholders with negative indices are ignored (no replacement, no drop).
- No special handling for method sends beyond normal call/pipe matching.
- Transforms run only in `.res`. Attachments in `.resi` will be carried in attrs but not executed.
- Only type constructor occurrences are replaced via `%replace.type`; no pattern matching over other type forms.
- Transform registry is minimal; most listed transforms are placeholders.

## Summary of Behavior

1. Collect deprecated uses from CMT (with optional templates and contexts).
2. Rewrite references/calls/pipes according to provided templates and placeholder rules.
3. Adjust labels, drop consumed args, and append inserted template args.
4. For `.res`, run a second pass applying any `@apply.transforms` attributes that were attached to expressions during rewriting.
5. Print updated AST; write file or stdout.
