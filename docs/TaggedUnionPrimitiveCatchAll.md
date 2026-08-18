Rationale
Consider a common interop type with a numeric discriminant where only a few literal values are special, and everything else should go to “Other”:

```res
@tag("kind")
type response =
  | @as(202) Ok202({code: int})
  | @as(200) Ok200({code: int})
  | @as(int) Other({kind: int, body: string})

let decode = (x: response) =>
  switch x {
  | Ok202(r) => r.code + 1
  | Ok200(r) => r.code + 3
  | Other(r) => r.kind + 2 // any other number lands here
  }
```

Before this feature, there was no elegant way to express “everything else that’s a number” for a tagged variant. You either had to hand‑write a decoder that inspects `x.kind` and constructs a variant by enumerating all literal cases yourself, or attempt exhaustive matches that still couldn’t represent unknown future values. Hand‑written decoding is tedious, error‑prone, and typically forces you to duplicate the discriminant logic outside the type just to avoid runtime match failures.

With a primitive catch‑all annotation like `@as(int|float|string)`, the type itself states the intent. The compiler generates literal‑first checks over the discriminant field and only then falls back to a single primitive “catch all” branch. This captures the real‑world use case (you only care about a few literals; everything else of that primitive kind is one case), avoids bespoke decoders, and stays sound even when a particular match expression doesn’t enumerate all literal members.

Title: Tagged-Union Primitive Catch‑All (@as(int|float|string))

Summary

- Allow a single catch‑all case per primitive kind in tagged unions declared with `@tag("…")`.
- Syntax: use `@as(int)`, `@as(float)`, or `@as(string)` on a payload constructor to denote: “match any value of this JS primitive type that isn’t one of the explicit literal tags.”
- Evaluation order: always test literal tags first; only if none match, test the primitive catch‑all of that kind.
- Mirrors existing behavior for `@unboxed` variants, but works for regular tagged variants as well.

Motivation

- Interop scenarios often have a numeric/string "discriminant" where only a handful of literal values have special meaning; everything else of that primitive kind should map to one “Other” constructor.
- Today we can model this for `@unboxed` variants. This proposal extends the same ergonomics to regular `@tag` variants.

Syntax and Examples

- Definition:
  @tag("kind")
  type response =
  | @as(202) Ok202({..})
  | @as(200) Ok200({..})
  | @as(int) Other({..})

- Allowed primitive catch‑alls: `int`, `float` (both map to JS `number`), `string`.
- Literals remain as today: numbers, strings, booleans, `null`, `undefined`.
- No catch‑all for `null`/`undefined` (they’re literals only).

Rules and Constraints

- One per kind: at most one primitive catch‑all per JS primitive kind in a single variant type:
  - number: at most one of `@as(int)` or `@as(float)` (they’re equivalent at runtime).
  - string: at most one `@as(string)`.
- Ordering: literal tags of a kind are matched first; the primitive catch‑all for that kind is tried only after all its literals fail.
- Applicability: only on constructors with payloads in `@tag("…")` variants. Rejected on nullary constructors and on `@unboxed` types (which already have their own mechanism).
- Soundness in matches: even if a match expression does not list all literal members, codegen must still treat all known literal tags as higher priority than the primitive catch‑all, so we never misroute a literal to the catch‑all.

Construction (Inline Record Only)

- Only inline records are allowed for primitive catch‑all constructors.
- Required tag field:
  - Exactly one field whose effective runtime name equals the `@tag` name (e.g., `"kind"`). Use `@as("kind")` on that field if its source name differs.
  - The field’s type must exactly match the catch‑all primitive: `int`, `float`, or `string` — used directly. No aliases, no abstract types, no expansion. The declared type must literally be one of the builtins.
  - The field must be required (non‑optional).
  - This specific tag‑name collision is allowed for the primitive catch‑all constructor; for all other constructors, the usual `TagFieldNameConflict` rule remains in force.
- No other payload shapes are permitted (no tuple payloads, no missing payload).
- Construction example:
  @tag("kind")
  type response =
  | @as(202) Ok202({code: int})
  | @as(200) Ok200({code: int})
  | @as(int) Other({kind: int, body: string})
  // Usage: Other({kind: 404, body: "Not Found"})

Runtime Semantics

- Representation is unchanged for normal cases. For primitive catch‑all cases we must provide the actual tag value at construction time via an inline record field whose runtime name equals the `@tag` name (details below). The compiler will not auto‑insert an extra tag property for such constructors (to avoid duplication).
- Pattern matching and switches:
  - Compute the discriminant expression `e` as the tag field (e.g., `x.kind`).
  - Guard: build a test that determines “is `e` one of the known literal tags?” using all type literals, not just the literals enumerated by the current match.
  - If guard passes, jump to the literal branch switch; otherwise, evaluate primitive catch‑all checks (typeof) per the declared catch‑alls.
  - For numbers, `int` and `float` share the same `typeof e === "number"` check. We still prefer direct equality against literal numeric tags before the typeof guard.

Parity With Unboxed Variants

- Ordering: identical to unboxed variants — first test all literal tags, then test primitive catch‑alls by `typeof`.
- Checks: reuse the existing DynamicChecks machinery; we simply apply it to the tag field (`E.tag ~name:tagName e`) instead of the raw value.
- Kinds: `int` and `float` collapse to JS `number`; `string` maps to JS `string`. `null`/`undefined` remain literals.
- Reuse over fork: we do not introduce a new matching strategy; we activate the existing unboxed dynamic path by making `block_cases` non‑empty for these constructors.

Compiler Changes

- Parsing/attributes (ML layer):
  - Extend `@as(...)` parsing to accept identifiers `int|float|string` in addition to existing literal payloads. Introduce an internal marker for “primitive catch‑all” on constructors, but do not reuse `Untagged ...` for tag value emission (that would emit the string "number").
- AST/plumbing:
  - In `names_from_type_variant`/`get_block_type`, surface a `block_type` for constructors annotated with a primitive catch‑all (int/float/string) even when the type is not `@unboxed`. This makes the compiler “see” that dynamic typeof checks are required for this tagged union.
  - Also carry a small bit on such constructors indicating how to retrieve the dynamic tag value at construction: "from field named tagName present in inline record".
- Well‑formedness checks:
  - Add checks (mirroring unboxed invariants) to ensure at most one catch‑all per primitive kind per variant type; treat `int` and `float` as the same “number” bucket.
  - Reject primitive catch‑alls on nullary constructors and when combined with `@unboxed`.
  - For inline record payloads with primitive catch‑all: require exactly one field whose effective runtime name equals the tag name (e.g., `"kind"`) and whose type matches the primitive; allow this specific tag‑name collision by relaxing `TagFieldNameConflict` in this case.
- Codegen (lambda → JS):
  - Reuse existing unboxed dynamic check machinery for `typeof` checks over the tag field expression (`E.tag ~name:tagName e`). This is already implemented via `DynamicChecks` in `ast_untagged_variants.ml` and used from `lam_compile` when `block_cases` is non‑empty.
  - Ensure literal guard uses all known literals from the type definition (via `get_literal_cases sw_names`), so missing cases in a match don’t misroute execution to the primitive catch‑all.
  - Value construction in `js_dump`:
    - For primitive catch‑all + inline record: do not auto‑insert the tag property; the inline record field renamed to the tag name supplies it.
- Error messages / pretty printing:
  - Update invalid `@as` diagnostics to include primitive identifiers `int|float|string` when used under `@tag` on payload constructors.
  - Improve duplicate‑primitive errors (e.g., “At most one number or one string catch‑all in this variant”).

Exhaustiveness and Soundness

- Exhaustiveness warnings behave as today. The presence of a primitive catch‑all does not automatically make matches exhaustive, because explicit literal tags still need coverage.
- JS generation remains sound: the literal‑first guard prevents a runtime value equal to a known literal (e.g., `200`) from being handled by the primitive catch‑all (e.g., `@as(int)`), even when the specific literal branch is not listed in the current match.

Edge Cases

- Overlap with literals: if a program constructs a value with a primitive tag equal to a declared literal (e.g., `Other` with `kind=200`), pattern matching still resolves to the literal branch due to guard ordering. We may optionally add a lint/warning when constructing such values.
- Numbers: `int` and `float` both map to JS `number`; we enforce “one number catch‑all” across both.
- Interop: behavior mirrors unboxed variant rules for typeof checks; `null`/`undefined` remain literal‑only.

Testing Plan

- Syntax tests: acceptance for `@as(int|float|string)` on payload constructors under `@tag`, and rejections on nullary or non‑tagged types.
- Lambda/JS IR tests: validate guard ordering, emitted typeof checks, and that unmatched literals route to the match default rather than the primitive catch‑all.
  - Construction: verify js_dump objects for inline record payloads set the tag from the provided value and do not duplicate it.
- End‑to‑end tests: typical interop scenarios with sparse literal coverage plus a catch‑all, including string/number cases and overlap with `null`/`undefined` literals.
- Negative cases:
  - Optional tag field (e.g., `@as("kind") kind?: int`) is rejected.
  - Missing tag field in the catch‑all constructor is rejected.
  - More than one field with effective tag name is rejected.
  - Duplicate primitive catch‑alls (e.g., both `@as(int)` and `@as(float)`) are rejected.

Implementation Notes (module touchpoints)

- compiler/ml/ast_untagged_variants.ml
  - Extend `process_tag_type` to recognize primitive identifiers; mark constructors as “primitive catch‑all (int|float|string)”.
  - In `get_block_type`, surface `Some <block_type>` for such constructors (even when not `@unboxed`).
  - In `check_invariant`, add “at‑most‑one per kind” checks for tagged primitive catch‑alls; relax `TagFieldNameConflict` when the constructor is a primitive catch‑all and the inline record contains the one required tag‑value field.
- compiler/core/lam_compile.ml
  - No structural change: `get_block_cases` becomes non‑empty when a tagged primitive catch‑all exists, which triggers reuse of the existing dynamic check path. The guard for literals vs. non‑literals is already implemented (`is_a_literal_case`).
- compiler/core/js_dump.ml
  - In `Blk_record_inlined` and `Blk_constructor` printing, add the construction rules above to set the tag from payload and avoid duplicating it.
- Error/help text updates in `report_error` and related pretty printers.

Backward Compatibility

- Existing code is unaffected. New syntax is opt‑in and does not change current matching or codegen for variants without primitive catch‑alls.

Open Questions

- Should we emit a warning when constructing a primitive catch‑all value whose tag equals a declared literal (e.g., `Other({kind: 200})`)? This is safe due to guard ordering but might be surprising.
- Should we support a convenience sugar for tuple payloads so that `Other(payload)` can be used and the tag value is pulled from a named payload field automatically? For now we keep the construction rules explicit.
