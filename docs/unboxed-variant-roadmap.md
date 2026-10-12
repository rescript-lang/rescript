# Unboxed variant extension roadmap

Candidate extensions in proposed implementation order. Each item can become one or more PRs.

Multiple named-record or inline-record cases, including mixtures, can be distinguished by a shared required, immutable runtime field. The comparison engine supports literal values and runtime types, with literal-first precedence when overlapping cases have compatible remaining payloads. This functionality is available by default.

For example:

```rescript
type createdKind = | @as("created") CreatedKind
type stoppedKind = | @as("stopped") StoppedKind
type created = {kind: createdKind, id: int}

@unboxed
type event =
  | Created(created)
  | Stopped({kind: stoppedKind, reason: string})
```

Both cases remain plain objects. Finite string domains retain field comparisons or switches; broader domains use strict equality, `typeof`, and instance checks. For example:

```rescript
@unboxed
type value =
  | Text({kind: string, text: string})
  | Number({kind: float, amount: float})
  | Flag({kind: bool, enabled: bool})

@unboxed
type message =
  | Created({kind: createdKind, value: int})
  | Other({kind: string, value: int})
```

`Other({kind: "created", value: 1})` is classified as `Created`. A match listing only `Other` and a wildcard therefore takes the wildcard. Such overlap is allowed only when all remaining fields have the same runtime names, types, optionality, and mutability. Otherwise the declaration is rejected, including when construction would supply its discriminator through a variable. Two overlapping literal sets or broad types remain ambiguous and are rejected; `int` and `float` share one JavaScript number domain. The chosen discriminator and its declaration-wide exclusions are part of interface and coercion compatibility.

Supported domains include string, numeric, bigint, boolean, null and undefined literals; primitive types; arrays/tuples; functions; ordinary objects; and recognized standard-library instances. Public variants can combine domains, erased single-field inline wrappers inherit their field's domain, and list fields account for both their empty and nonempty representations. `unit` has the singleton undefined domain. Typed-array aliases share one typed-array family; they cannot distinguish individual typed-array constructors. Type aliases, concrete generic-record substitutions, and `@as` field names are supported.

Optional or mutable discriminators, unresolved/abstract representations, option's special representation (including locally declared `Some`/`None` variants), GADT discriminator types, field-presence tests, nested/multiple discriminator fields, and named outer payload records emitted as arrays remain unsupported. Outer GADT constructors can use disjoint discriminator domains, but literal-first overlap involving GADT constructors is rejected to preserve their type refinements. There is no new `@catch` syntax or pattern-only constructor restriction. Future catch-all support can reuse domain exclusions while adding its own construction rules; see [the original proposal](https://github.com/rescript-lang/rescript/pull/7996) and [its pattern-only successor](https://github.com/rescript-lang/rescript/pull/8306).

- **Preserve the existing path:** keep current runtime layouts, matching, constant folding, and JavaScript generation for already-supported declarations. Route newly supported cases through object-discriminator layouts and handling.
- **Check module compatibility:** verify interface consistency, coercions, cross-module inlining, and transitive dependencies.
- **Require sound discrimination:** reject ambiguous or unsupported shapes. Any permitted overlap needs explicit, consistent semantics across matching and optimization.
- **Provide actionable diagnostics:** identify conflicting constructors, relevant field paths, and the reason discrimination fails; suggest a concrete fix. Follow `super_errors` and `super_errors_multi` precedents and keep `tests/ERROR_VARIANTS.md` current.
- **Test every extension:** cover accepted and rejected declarations, runtime behavior, generated JavaScript, exhaustiveness, guards, and agreement between optimized and non-folded matches. Include aliases, optional and renamed fields, recursive and generic types, interfaces, and coercions where relevant.
- **Verify existing behavior:** preserve JavaScript snapshots for already-supported declarations and diagnostics for unrelated errors. Exercise incremental/watch rebuilds and compiler/runtime performance on large shapes using the existing test infrastructure.
- **Apply repository checks per PR:** run `make test`, plus syntax, roundtrip, genType, analysis, and Rewatch suites as affected; include the required changelog entry for user-facing changes.

1. **Object literal discriminants:** implemented for finite scalar literal domains, with shared runtime equality and overlap rules.
2. **Object field type discriminants:** implemented for recognized runtime domains, including safe literal-first precedence and declaration-wide exclusions.
3. **Nested and multi-field discriminants:** distinguish shapes using nested fields or combinations of fields.
4. **Field presence and absence:** distinguish shapes with explicit rules for optional fields and extra properties.
5. **Explicit fallback cases:** support broader catch-all payloads with defined precedence for more specific shapes.
6. **Tuple lengths:** allow multiple tuple cases distinguished by length.
7. **Tuple element discriminants:** distinguish equal-length tuples through element literals or runtime types.
8. **Compositional unboxing:** derive shapes through nested variants, finite enums, and unboxed wrappers.
9. **Mixed representations:** support tagged object cases alongside unboxed payload cases consistently.
10. **Extensible instance recognition:** support JavaScript instance types beyond the built-in list.
11. **Broader coercions:** expand representation-preserving conversions between payloads, primitives, and variants.
12. **Generic variant composition:** support parameterized spreads and combinations whose runtime shapes remain distinguishable.
