# Compiler error variant catalog

A per-module table of every named error and warning variant the compiler
declares, tagged with whether a test fixture currently exercises it.

The catalog has two practical uses:

1. **Coverage expansion** — find rows tagged `☐` (reachable but no
   fixture) and add a `super_errors` / `super_errors_multi` fixture for
   each. The Notes column points at the trigger site and any required
   AST shape.
2. **Dead-code removal** — rows tagged `⚠` are variants whose trigger
   site is unreachable in the current parser / compiler, with a named
   blocker. They can be deleted in a follow-up PR.

## Status legend

| Symbol | Meaning |
|---|---|
| ✓ | Covered by at least one fixture under `tests/build_tests/super_errors/` or `tests/build_tests/super_errors_multi/`. |
| ⚠ | **Verified** unreachable: the trigger site has a specific blocker named in source (an exception declared but never raised, an AST node the parser doesn't construct, a guard that's always false). Candidate for dead-code removal. |
| ☐ | Reachable but no fixture yet; would be valuable to add. |
| ? | Trigger site is live but reachability from regular ReScript source isn't confirmed. Distinct from ⚠: a `?` means "I couldn't find a fixture that reaches it" rather than "the path is provably blocked". |

The removal audit section below records variants that have already been
deleted or retained after re-validation.

## Scope

This catalog enumerates **named error variants** — constructors of
`type error = …` declarations across `compiler/`. The compiler also has
~94 `Location.raise_errorf` and similar inline calls that produce
user-facing errors without a named variant. Those are **not** catalogued
here because there's no constructor to track. If someone wants to add
coverage for one of those messages, the first step is refactoring it
into a named variant.

## Fixture paths

- Single-file fixtures live in [`build_tests/super_errors/fixtures/`](build_tests/super_errors/fixtures/) with expected output in [`build_tests/super_errors/expected/`](build_tests/super_errors/expected/).
- Multi-file fixtures live in [`build_tests/super_errors_multi/fixtures/<FixtureName>/`](build_tests/super_errors_multi/fixtures/) with expected output in [`build_tests/super_errors_multi/expected/<FixtureName>.expected`](build_tests/super_errors_multi/expected/).

## How to update

When you add or remove an error variant in `compiler/`, update the
corresponding row here too:

1. Add (or remove) the row in the appropriate module section below.
2. Write a fixture if the variant is reachable. Use `super_errors/` for
   single-file scenarios, `super_errors_multi/` for cross-file ones.
3. Snapshot with `node tests/build_tests/super_errors{,_multi}/input.js update`.
4. Set the status column accordingly.

If a variant turns out to be unreachable, document the named blocker
here (so it gets ⚠ instead of `?`) and file a follow-up to delete the
dead code.

## Removed in `jono/remove-dead-errors`

Two classes of removal. **Truly dead** variants — no raise site under
`compiler/`, or a guard whose predicate is always false — are deleted
entirely (declaration, raise, reporter, supporting helpers). **Defensive
unreachable** variants whose raise sites exist but are unreachable from
the ReScript parser keep their named-variant form removed and the raise
site is replaced with `assert false (* reason *)`, matching the
established convention in the typer modules (57 existing uses).

**Truly dead — declaration + raise + reporter all removed:**

- `ctype`: `Tags`, `Recursive_abbrev`,
  `Unification_recursive_abbrev` exceptions (declared, never raised)
- `typecore`: `Recursive_local_constraint` (relay wrapper for the dead
  `Unification_recursive_abbrev`)
- `typetexp`: `Variant_tags` (relay wrapper for the dead `Tags`)
- `bs_syntaxerr`: `Conflict_bs_bs_this_bs_meth` (no construction site)
- `warnings`: `Comment_start`, `Comment_not_end`, `Method_override`,
  `Instance_variable_override`, `Illegal_backslash`,
  `Implicit_public_methods`, `Eol_in_string`,
  `Eliminated_optional_arguments`, `Bad_docstring`,
  `Bs_fragile_external`, `Bs_unimplemented_primitive` (declared,
  never raised); `Statement_type` (caller hard-codes `statement=false`),
  `Unerasable_optional_argument` (disabled around its only check),
  `Bs_uninterpreted_delimiters` (trigger AST rewritten by builtin PPX
  before the warning iterator runs)

**Defensive unreachable — raise site becomes `assert false (* reason *)`,
variant + reporter removed:**

- `typecore`: `Invalid_for_of_pattern` (parser's
  `normalize_for_of_pattern` replaces non-var patterns with `Ppat_any`,
  so the typer only ever sees `Ppat_var`/`Ppat_any` in for-of bindings)
- `typetexp`: `Ill_typed_functor_application`, `Apply_structure_as_functor`
  (both need `Longident.Lapply`, never constructed by the parser —
  `compiler/syntax/src/` only builds `Lident`/`Ldot`)
- `typedecl`: `Val_in_structure` (`pval_prim = []` / a bare `val` outside
  a signature is only produced by the parser's external-recovery, which
  emits a syntax error and aborts before the typer runs)
- `bs_syntaxerr`: `Unhandled_poly_type` (its only raise site is in
  `ast_core_type.list_of_arrow`; `Ptyp_poly` cannot appear in an
  external's arrow chain — external types never route through
  `parse_poly_type_expr`, and any `Ptyp_poly` in a record/object field is
  an opaque arg leaf, never the recursed return type)

> ⚠️ **Reachability re-audit correction.** An earlier pass mislabelled
> several of these as unreachable on plausible-but-untested reasoning. A
> follow-up audit (empirical: each `assert false` was driven to a real
> crash with a minimal `.res`) found **8** that are actually reachable or
> unproven, and they were restored as named variants (see the retained
> list below). **7 of the 8 now have a reproducing fixture** confirming
> they are live; the last (`typedecl.Type_clash`) resisted ~37 attempts
> and has a structural argument for unreachability but no airtight proof,
> so it is retained rather than removed. The remaining entries above are
> backed by either a parser grammar fact or a structural proof, not just a
> guess.

Re-validation found a number of previously-flagged items that are not
completely dead and have been retained as named variants:

- `typedecl.Rebind_wrong_type` — live via extension rebinding across
  incompatible extension types.
- `typecore.Label_mismatch` — live when a record literal without an
  expected type mixes fields from two different record types
  (`label_mismatch_record_literal.res`).
- `typecore.Abstract_wrong_label` — live when a multi-arg function
  literal has a mislabelled inner argument (`abstract_wrong_label.res`).
- `typemod.With_cannot_remove_constrained_type` — live when destructive
  substitution is applied to a constrained type
  (`with_cannot_remove_constrained_type.res`).
- `typetexp.Unbound_type_constructor_2` — live when an identity type
  alias `type t<'a> = 'a` is used in an inherit position with a
  type-variable argument: `expand_head` collapses `t<'b>` to a bare
  `Tvar` while the unexpanded repr is still `Tconstr`. Reachable from
  both poly-variant inherit
  (`incomplete_type_constructor_polyvariant.res`) and object spread
  (`incomplete_type_constructor_object.res`). The earlier
  "parser-rejected leading-`'a`" reasoning was wrong; both raise sites
  reproduce as a compiler crash if removed.
- `typecore.Invalid_interval` — live: the parser **does** construct
  `Ppat_interval` (`res_core.ml`, `<const> .. <const>`). Only the
  `Pconst_char` interval is rewritten; any other constant interval (e.g.
  `1 .. 5`) reaches this branch (`pattern_interval_non_char.res`).
- `typedecl.Null_arity_external` — live: an external whose name starts
  with `?` (e.g. `external x: int = "?nodeFs"`) skips the magic
  `prim_native_name` encoding and reaches the typer with arity 0
  (`external_null_arity.res`).
- `typedecl.Varying_anonymous` — live: a **variance annotation** on a
  GADT parameter whose return type constrains it (e.g.
  `type rec t<+'a> = K(int): t<int>`) triggers it — not `_` params
  (`gadt_varying_anonymous.res`).
- `typedecl.Parameters_differ` — live: non-uniform recursion through an
  object/record manifest (e.g. `type rec t<'a> = {"f": t<int>}`) is
  caught by `check_regular`, not `Cycle_in_def`
  (`recursive_type_parameters_differ.res`).
- `typedecl.Bad_fixed_type` — live: a fully-bounded closed private
  polymorphic variant (e.g. `type t = private [< #A | #B > #A #B]`)
  satisfies `is_fixed_type` but has a static (non-`Tvar`) row
  (`fixed_type_no_row_variable.res`).
- `env.Illegal_value_name` — live: escaped-identifier syntax reaches
  `check_value_name` during definition; `let \"->" = 1` is rejected with
  a clean diagnostic (`illegal_value_name.res`). The parser does **not**
  reject `\"->"`.
- `typecore.Incoherent_label_order` — live: a not-yet-generalized
  function value applied more than once with labelled arguments in
  conflicting orders (`let f = g => (g(~a=1, ~b=2), g(~b=3, ~a=4))`) hits
  the leftover/tvar path in `type_unknown_args` after the first call fixes
  the arrow order (`labeled_args_incoherent_order.res`).
- `typedecl.Type_clash` — retained but **appears dead**: its only raise
  site (`update_type`) unifies `t<fresh params>` against `t`'s own
  manifest — a type against an alpha-renamed copy of itself — which cannot
  head-clash, and every genuine inconsistency is caught by a dedicated
  check (`Cycle_in_def`, `Recursive_abbrev`, `Parameters_differ`,
  `Constraint_failed`, `Type_arity_mismatch`). ~37 recursive / mutual /
  constraint / row / object / alias shapes were tried without reaching it.
  Kept as a named variant with an explanatory comment at the definition
  and raise site, pending an airtight proof, rather than `assert false`.
- `bs_syntaxerr.Misplaced_label_syntax` — live when labelled args are
  passed via operator-identifier syntax like `\"->"(x, ~b=...)`
  (`misplaced_label_syntax.res`).
- `typemod.Cannot_eliminate_dependency`, `typemod.Scoping_pack`,
  `typemod.With_makes_applicative_functor_ill_typed` — reproduction
  attempts couldn't reach them but couldn't conclusively prove
  unreachability either; retained on the conservative side.
- `includemod.Unbound_modtype_path` can still represent a stale
  compiled-interface failure.
- `Syntaxerr.Variable_in_scope` is live but lacks a registered printer.
- The UTF-8 helper error families are still raised by test/defensive
  helper entry points.

---

## `compiler/ml/typecore.ml`

The largest error type; covers expression / pattern type-checking.
Source: [typecore.ml:27](../compiler/ml/typecore.ml).

| Variant | Status | Fixture | Notes |
|---|---|---|---|
| `Polymorphic_label` | ✓ | `polymorphic_label.res` | Pattern that instantiates a polymorphic record field: `({f: (f: int => int)}: t) =>` constrains the universal `'a` of `f: 'a. 'a => 'a` to `int => int`. |
| `Constructor_arity_mismatch` | ✓ | `constructor_arity_mismatch.res`, `constructor_arity_mismatch_pattern.res`, `arity_mismatch*.res` | Triggers in both expression (4028) and pattern (1426) paths. |
| `Label_mismatch` | ✓ | `label_mismatch_record_literal.res` | Record literal without expected type mixing fields from two different record types — disambiguation picks one type per label, and the cross-type unify fails inside `type_label_exp`. |
| `Pattern_type_clash` | ✓ | many `*_pattern_type_clash.res` etc. | Most-fired pattern error. Sub-case fixtures: `pattern_matching_on_option_but_value_not_option.res` and `pattern_matching_on_value_but_is_option.res` (option-vs-non-option trace), `pattern_type_clash_polyvariant.res` (polyvariant tag against concrete type), `pattern_type_clash_tuple_arity.res` (tuple arity mismatch). |
| `Or_pattern_type_clash` | ✓ | `or_pattern_type_clash.res` | |
| `Multiply_bound_variable` | ✓ | `multiply_bound_variable.res` | |
| `Orpat_vars` | ✓ | `orpat_vars_unbalanced.res` | |
| `Expr_type_clash` | ✓ | many `*.res` | Most-fired expression error. Trace-shape sub-cases covered: `if_return_type_mismatch.res` (IfReturn), `maybe_unwrap_option.res` (MaybeUnwrapOption), `string_concat_non_string.res` (StringConcat), `labeled_fn_argument_type_clash.res` (FunctionArgument with explicit label), `math_operator_*.res` (MathOperator family), `ternary_branch_mismatch.res`, `switch_different_types.res`, `try_catch_same_type.res`, `comparison_operator.res`, `array_item_type_mismatch.res`, `array_literal_passed_to_tuple.res`, `if_condition_mismatch.res`, `while_condition.res`, `for_loop_condition.res`, `assert_condition.res`, `function_call_mismatch.res`, `awaiting_non_promise.res`, multiple `jsx_*` fixtures. |
| `Apply_non_function` | ✓ | `apply_non_function.res` | |
| `Apply_wrong_label` | ✓ | `apply_wrong_label.res` | |
| `Label_multiply_defined` | ✓ | `label_multiply_defined_literal.res` | |
| `Labels_missing` | ✓ | `missing_label.res`, `missing_labels.res` | |
| `Label_not_mutable` | ✓ | `label_not_mutable.res` | |
| `Wrong_name` | ✓ | `wrong_name_record_field.res`, `Cross_record_extra_field` (multi) | |
| `Name_type_mismatch` | ✓ | `super_errors_multi/Cross_qualified_constructor_mismatch` | Cross-module constructor disambiguation. |
| `Undefined_method` | ✓ | `super_errors_multi/Cross_module_alias_dot_access`, `undefined_method` | |
| `Private_type` | ✓ | `private_type_construction.res` | |
| `Private_label` | ✓ | `private_label.res` | |
| `Not_subtype` | ✓ | `subtype_*.res`, `dict_show_no_coercion.res`, etc. | |
| `Too_many_arguments` | ✓ | `too_many_arguments.res`, `moreArguments*.res` | |
| `Abstract_wrong_label` | ✓ | `abstract_wrong_label.res` | Multi-arg function literal where an inner argument label doesn't match the expected arrow's label (e.g. `let f: (~a, ~b) => int = (~a, ~c) => …`). |
| `Scoping_let_module` | ✓ | `scoping_let_module.res` | |
| `Not_a_variant_type` | ✓ | `variant_spread_pattern_not_a_variant.res` | Pattern-level variant spread of a non-variant type. |
| `Incoherent_label_order` | ✓ | `labeled_args_incoherent_order.res` | A not-yet-generalized function value applied more than once with labelled args in conflicting orders (`g => (g(~a, ~b), g(~b, ~a))`); the reordered second call hits the leftover/tvar path in `type_unknown_args`. |
| `Less_general` | ✓ | `less_general_universal.res` | |
| `Modules_not_allowed` | ✓ | `super_errors_multi/Modules_not_allowed_toplevel` | Toplevel `let module(M) = …` pattern with `allow_modules=false`. |
| `Cannot_infer_signature` | ✓ | `cannot_infer_signature.res` | |
| `Not_a_packed_module` | ✓ | `not_a_packed_module.res` | |
| `Unexpected_existential` | ✓ | `super_errors_multi/Unexpected_existential_in_let` | Destructuring GADT constructor with existential in toplevel `let`. |
| `Unqualified_gadt_pattern` | ✓ | `super_errors_multi/Cross_gadt_pattern` | Only reachable via cross-module GADT disambiguation; in single-file matching the constructor would resolve before this check. |
| `Invalid_interval` | ✓ | `pattern_interval_non_char.res` | Non-char constant interval pattern (e.g. `1 .. 5`); the parser builds `Ppat_interval` and only the `Pconst_char` interval is rewritten. |
| `Invalid_for_loop_index` | ✓ | `invalid_for_loop_index.res` | |
| `No_value_clauses` | ✓ | `no_value_clauses.res` | |
| `Exception_pattern_below_toplevel` | ✓ | `exception_pattern_below_toplevel.res` | |
| `Inlined_record_escape` | ✓ | `inline_record_escape.res` | |
| `Inlined_record_expected` | ✓ | `inlined_record_expected.res`, `super_errors_multi/Cross_inline_record_constructor` | |
| `Invalid_extension_constructor_payload` | ✓ | `invalid_extension_constructor_payload.res` | |
| `Not_an_extension_constructor` | ✓ | `not_an_extension_constructor.res` | |
| `Break_outside_loop` | ✓ | `break_outside_loop.res`, `break_in_nested_function.res` | |
| `Continue_outside_loop` | ✓ | `continue_outside_loop.res`, `continue_in_nested_function.res` | |
| `Literal_overflow` | ✓ | `intoverflow.res` | |
| `Unknown_literal` | ✓ | `unknown_literal.res` | |
| `Illegal_letrec_pat` | ✓ | `illegal_letrec_pat.res` | |
| `Empty_record_literal` | ✓ | `empty_record_literal.res` | |
| `Uncurried_arity_mismatch` | ✓ | `arity_mismatch3.res` etc. | |
| `Field_not_optional` | ✓ | `fieldNotOptional.res` | |
| `Record_rest` | ✓ | `record_rest_*.res` | Wrapper for record-rest validation errors reported by `typecore_record_rest.ml`; fixtures cover missing annotation, invalid rest type, non-record and unresolved rest types, private rest type, field mismatch/missing/extra cases, runtime-name mismatch, empty-rest warning, module destructure rejection, and singular/plural missing and overlap messages. |
| `Type_params_not_supported` | ✓ | `variant_spread_pattern_type_params.res` | Pattern-level variant spread (`| ...a as v`) where `a` has type params; typedecl path covered by `variant_spread_type_parameters.res`. |
| `Field_access_on_dict_type` | ✓ | `field_access_on_dict_type.res` | |
| `Jsx_not_enabled` | ☐ (needs harness flag) | — | typecore.ml:218/3470. Fires when JSX is used without `-bs-jsx N`. The `super_errors` runner hard-codes `-bs-jsx 4` in `bscFlags`; adding a per-fixture opt-out (e.g. a `.opts` sidecar) would expose this. Until then, it's reachable in real code but blocked at the harness level. |
| `Tagged_template_non_tag` | ✓ | `tagged_template_non_tag.res` | Backtick tagged-template syntax used on a value whose type is not `taggedTemplate<'param, 'output>`. |

---

## `compiler/ml/typedecl.ml`

Type-declaration errors. Source: [typedecl.ml:27](../compiler/ml/typedecl.ml).

| Variant | Status | Fixture | Notes |
|---|---|---|---|
| `Repeated_parameter` | ✓ | `repeated_type_parameter.res` | |
| `Duplicate_constructor` | ✓ | `duplicate_variant_constructor.res` | |
| `Duplicate_label` | ✓ | `duplicate_labels_error.res` | |
| `Object_spread_with_record_field` | ✓ | `object_spread_with_record_field.res` | |
| `Recursive_abbrev` | ✓ | `recursive_type_abbreviation.res`, `recursive_type.res` | |
| `Cycle_in_def` | ✓ | `recursive_type_abbrev_cycle.res` | |
| `Definition_mismatch` | ✓ | `definition_mismatch.res` | |
| `Constraint_failed` | ✓ | `constraint_failed.res` | |
| `Inconsistent_constraint` | ✓ | `inconsistent_constraint.res` | |
| `Type_clash` | ⚠ | — | Appears dead; retained pending proof. `update_type` unifies `t<fresh>` against `t`'s own manifest (an alpha-renamed copy of itself), which can't head-clash; real inconsistencies hit `Cycle_in_def`/`Recursive_abbrev`/`Parameters_differ` first. ~37 shapes tried without reaching it. |
| `Parameters_differ` | ✓ | `recursive_type_parameters_differ.res` | Non-uniform recursion through an object/record manifest (`type rec t<'a> = {"f": t<int>}`), caught by `check_regular` rather than `Cycle_in_def`. |
| `Null_arity_external` | ✓ | `external_null_arity.res` | External whose name starts with `?` (e.g. `external x: int = "?nodeFs"`) skips the magic `prim_native_name` encoding and reaches the typer with arity 0. |
| `Unbound_type_var` | ✓ | `unbound_type_var.res` | |
| `Cannot_extend_private_type` | ✓ | `cannot_extend_private_type.res` | |
| `Not_extensible_type` | ✓ | `not_extensible_type.res` | |
| `Extension_mismatch` | ✓ | `extension_arity_mismatch.res` | `type t<'a> = ..` extended with `type t += A(int)` — arity differs from the extensible type. |
| `Rebind_wrong_type` | ✓ | `extension_rebind_mismatch.res` | Rebinding constructor into a different extensible type fails while unifying the source constructor result with the extension target. |
| `Rebind_mismatch` | ? | — | The later declaration-shape check after `Rebind_wrong_type`; no source fixture was confirmed in this pass. |
| `Rebind_private` | ✓ | `extension_rebind_private.res` | Rebinding a private extension constructor as public. |
| `Bad_variance` | ✓ | `bad_variance.res`, `bad_variance_contra.res` | |
| `Unavailable_type_constructor` | ☐ (needs build harness) | — | typedecl.ml:778. Requires a type path findable at parse time but missing during constraint enforcement; only cross-unit scenarios where a `.cmi` was found but later removed. |
| `Bad_fixed_type` | ✓ | `fixed_type_no_row_variable.res` | Fully-bounded closed private polymorphic variant (`type t = private [< #A | #B > #A #B]`) satisfies `is_fixed_type` but has a static (non-`Tvar`) row. |
| `Unbound_type_var_ext` | ✓ | `unbound_type_var_extension.res` | |
| `Varying_anonymous` | ✓ | `gadt_varying_anonymous.res` | Variance annotation on a GADT parameter whose return type constrains it (`type rec t<+'a> = K(int): t<int>`). |
| `Invalid_attribute` | ✓ | `invalid_attribute_not_undefined.res` | |
| `Bad_immediate_attribute` | ✓ | `bad_immediate_attribute.res` | |
| `Bad_unboxed_attribute` | ✓ | `bad_unboxed_attribute_abstract.res`, `bad_unboxed_attribute_mutable.res`, `bad_unboxed_attribute_many_fields.res`, `bad_unboxed_attribute_extensible.res` | All 4 sub-cases covered. |
| `Boxed_and_unboxed` | ✓ | `boxed_and_unboxed.res` | |
| `Nonrec_gadt` | ✓ | `nonrec_gadt.res` | |
| `Variant_runtime_representation_mismatch` | ✓ | `variant_coercion_*.res` (many) | |
| `Variant_spread_fail` | ✓ | `variant_spread_*.res` (many), `variant_spread_non_variant.res` | |

---

## `compiler/ml/typemod.ml`

Module-level errors. Source: [typemod.ml:24](../compiler/ml/typemod.ml).

| Variant | Status | Fixture | Notes |
|---|---|---|---|
| `Cannot_apply` | ✓ | `cannot_apply_non_functor.res` | |
| `Not_included` | ✓ | All `super_errors_multi/Iface_*` fixtures wrap to this via `compunit`. | |
| `Signature_expected` | ✓ | `typemod_signature_expected.res` | `with type M.t = …` where `M` is functor-typed inside the outer signature. |
| `Structure_expected` | ✓ | `super_errors_multi/Smoke_unbound_module_reference` (indirect); also `open_functor.res` | |
| `With_no_component` | ✓ | `with_no_component.res` | |
| `With_mismatch` | ✓ | `with_mismatch.res` | |
| `With_makes_applicative_functor_ill_typed` | ? | — | typemod.ml:249. Fires when a `with` constraint on a signature containing a `Path.Papply` makes that application ill-typed. Reachable via destructive substitution on applicative functors; reproduction attempts surfaced `not a signature` first. Retained on the conservative side. |
| `With_cannot_remove_constrained_type` | ✓ | `with_cannot_remove_constrained_type.res` | Destructive substitution on a constrained type, e.g. `S with type t<'a> := 'a` where `S` has `type t<'a> constraint 'a = int`. |
| `With_changes_module_alias` | ☐ (needs build harness) | — | typemod.ml:240. Fires during `with module := M2` substitution when an aliased sub-module inside the constrained signature is affected. ReScript parses `with module N := M2` (destructive substitution), but constructing a sub-module alias chain that gets invalidated requires multiple `.resi` files and a specific shape I couldn't reproduce single-file. |
| `Cannot_eliminate_dependency` | ? | — | typemod.ml:1332. Fires when `Mtype.nondep_supertype` raises `Not_found` during functor-application result-type computation. Reproduction attempts routed through `Incomplete_packed_module` or `escapes its scope`. Retained on the conservative side. |
| `Scoping_pack` | ? | — | typemod.ml:1717. Fires during first-class module packing with `with type` constraints whose constrained type isn't a free `Tvar`. Reproduction attempts routed through `Incomplete_packed_module`. Retained on the conservative side. |
| `Repeated_name` | ✓ | `repeated_def_*.res` (multiple) | |
| `Non_generalizable` | ✓ | `non_generalizable.res` | |
| `Non_generalizable_module` | ✓ | `non_generalizable_module.res` | Nested module containing `let r = ref(None)` — the outer module's `md_type` carries the free `'_weak1` from the inner ref, so `closed_modtype` returns false and the `Sig_module` branch fires. |
| `Interface_not_compiled` | ✓ | `super_errors_multi/Iface_not_compiled` | |
| `Not_allowed_in_functor_body` | ✓ | `super_errors_multi/not_allowed_in_functor_body` (TODO: confirm path) | |
| `Not_a_packed_module` | ✓ | `not_a_packed_module.res` | |
| `Incomplete_packed_module` | ✓ | `incomplete_packed_module.res` | |
| `Recursive_module_require_explicit_type` | ✓ | `recursive_module_require_explicit_type.res` | |
| `Apply_generative` | ✓ | `apply_generative.res` | |
| `Cannot_scrape_alias` | ☐ (needs build harness) | — | typemod.ml:77, 83, 1347. Requires `Env.scrape_alias` to return `Mty_alias` for an alias whose target `.cmi` couldn't be loaded. The `super_errors_multi` runner pre-compiles every file in the fixture, so the alias target is always present. |

---

## `compiler/ml/typetexp.ml`

Type-expression errors. Source: [typetexp.ml:28](../compiler/ml/typetexp.ml).

| Variant | Status | Fixture | Notes |
|---|---|---|---|
| `Unbound_type_variable` | ✓ | (covered indirectly via many fixtures) | |
| `Unbound_type_constructor` | ✓ | `typetexp_unbound_type_constructor.res` | |
| `Unbound_type_constructor_2` | ✓ | `incomplete_type_constructor_polyvariant.res`, `incomplete_type_constructor_object.res` | Identity alias `type t<'a> = 'a` used in an inherit position with a type-variable arg; `expand_head` collapses `t<'b>` to a bare `Tvar` while the repr stays `Tconstr`. Reachable from poly-variant inherit and object spread. |
| `Type_arity_mismatch` | ✓ | `type_arity_mismatch.res`, `record_rest_type_arity_mismatch.res` | |
| `Type_mismatch` | ✓ | `typetexp_type_mismatch.res` | Type-constructor application that violates a `constraint 'a = …` on the declaration. |
| `Alias_type_mismatch` | ✓ | `typetexp_alias_type_mismatch.res` | |
| `Present_has_conjunction` | ✓ | `polyvariant_present_has_conjunction.res` | `[< #A(int) & (string) > #A]` — `<` syntax marks `#A` as a "present" tag, and the body has both `(int)` and `& (string)` types, so the conjunctive payload triggers the check at line 451. |
| `Present_has_no_type` | ✓ | `polyvariant_present_has_no_type.res` | `[< #B > #A]` — `#A` is listed as a "present" tag but isn't defined in the polyvariant body. |
| `Constructor_mismatch` | ✓ | `polyvariant_constructor_mismatch.res` | |
| `Not_a_variant` | ✓ | `typetexp_not_a_variant.res` | Polyvariant `[#X \| a]` where `a` is not a polyvariant. |
| `Invalid_variable_name` | ✓ | `invalid_type_variable_name.res` | |
| `Cannot_quantify` | ✓ | `cannot_quantify.res` | `type t = {f: 'a. (int as 'a) => int}` — `'a` is universally quantified but the alias `int as 'a` rebinds it to `int`, so the proxy is no longer a fresh `Tvar` when the quantification check runs. |
| `Multiple_constraints_on_type` | ✓ | `multiple_constraints_on_type.res` | |
| `Method_mismatch` | ✓ | `object_method_mismatch.res` | |
| `Unbound_value` | ✓ | `typetexp_unbound_value.res` | |
| `Unbound_constructor` | ✓ | `typetexp_unbound_constructor.res` | |
| `Unbound_label` | ✓ | `typetexp_unbound_label.res` | |
| `Unbound_module` | ✓ | `suggest_module_for_missing_identifier.res`, `super_errors_multi/Smoke_unbound_module_reference` | |
| `Unbound_modtype` | ✓ | `typetexp_unbound_modtype.res` | |
| `Illegal_reference_to_recursive_module` | ✓ | `illegal_recursive_module_reference.res` | `module rec A: B.S = …` references another recmodule's module-type before signatures are sealed. During `approx_modtype` of A, `Env.lookup_module B` returns the `#recmod#` placeholder and raises `Env.Recmodule`. |
| `Access_functor_as_structure` | ✓ | `access_functor_as_structure.res` | |
| `Cannot_scrape_alias` | ☐ (needs build harness) | — | typetexp.ml:86 (Ldot path, live), 95/101 (Lapply path, dead since `Lapply` isn't parsed). The live Ldot trigger needs `Env.scrape_alias` to return `Mty_alias` — an alias whose target `.cmi` couldn't be loaded. The `super_errors_multi` harness pre-compiles every alias target. |
| `Opened_object` | ✓ | `object_inherit_opened.res` | |
| `Not_an_object` | ✓ | `object_inherit_not_an_object.res` | |

---

## `compiler/ml/includemod.ml` (symptom)

Wrapper symptoms attached to inclusion failures. Source: [includemod.ml:23](../compiler/ml/includemod.ml).

| Variant | Status | Fixture | Notes |
|---|---|---|---|
| `Missing_field` | ✓ | `super_errors_multi/Iface_missing_value` | |
| `Value_descriptions` | ✓ | `super_errors_multi/Iface_value_descriptions`, `super_errors_multi/Smoke_interface_mismatch` | |
| `Type_declarations` | ✓ | `super_errors_multi/Iface_type_decl_record`, `super_errors_multi/Iface_type_decl_variant`, `RecordInclusion.res` | |
| `Extension_constructors` | ✓ | `super_errors_multi/Iface_extension_constructors` | |
| `Module_types` | ✓ | `super_errors_multi/Iface_module_types` | |
| `Modtype_infos` | ✓ | `super_errors_multi/Iface_modtype_infos` | |
| `Modtype_permutation` | ✓ | `super_errors_multi/include_modtype_permutation` | |
| `Interface_mismatch` | ✓ | wrapper added to all `Iface_*` failures (line 476). | |
| `Unbound_modtype_path` | ☐ (needs stale-cmi harness) | — | includemod.ml:94. Re-validated during removal: not completely dead. It represents `Env.find_modtype` failing during module-type path comparison, which can happen only with a stale or inconsistent compiled interface. The source-only harness cannot produce that state because it pre-compiles every fixture, so this needs a build/binary-state harness rather than deletion. |
| `Unbound_module_path` | ☐ (needs build harness) | — | includemod.ml:226/233. Alias comparison where `Env.normalize_path` raises `Not_found`. Requires a module alias whose target `.cmi` is absent at inclusion time — multi-unit only. |
| `Invalid_module_alias` | ☐ (needs build harness) | — | includemod.ml:211. Requires both sides `Mty_alias` with one pointing to a functor argument. Reachable only when the alias chain crosses a functor application that the `super_errors_multi` harness doesn't construct. |

---

## `compiler/ml/includecore.ml` (`type_mismatch`)

Sub-symptoms produced during signature inclusion (rendered inside `Type_declarations`).
Source: [includecore.ml:159](../compiler/ml/includecore.ml).

| Variant | Status | Fixture | Notes |
|---|---|---|---|
| `Arity` | ✓ | `definition_mismatch.res` | |
| `Privacy` | ✓ | `super_errors_multi/Iface_privacy_mismatch` | |
| `Kind` | ✓ | `super_errors_multi/Iface_kind_mismatch` | Record-in-impl vs variant-in-interface. |
| `Constraint` | ✓ | `super_errors_multi/Iface_constraint_mismatch` | Implementation adds a `constraint 'a = …`; interface has none. |
| `Manifest` | ✓ | `super_errors_multi/Iface_manifest_mismatch` | Manifest types differ (`int` vs `string`). |
| `Variance` | ✓ | `super_errors_multi/Iface_variance_mismatch` | Interface annotates `+'a`; implementation's inferred variance differs. |
| `Field_type` | ✓ | `super_errors_multi/Iface_type_decl_record` | |
| `Field_mutable` | ✓ | `super_errors_multi/Iface_field_mutable_mismatch` | |
| `Field_optional` | ✓ | `super_errors_multi/Iface_field_optional_mismatch` | |
| `Field_arity` | ✓ | `super_errors_multi/Iface_field_arity_mismatch` | Constructor with different argument count between `.resi` / `.res`. |
| `Field_names` | ✓ | `super_errors_multi/Iface_field_names_mismatch` | Record field names differ at the same position. |
| `Field_missing` | ✓ | `super_errors_multi/Iface_missing_value` (indirect) | |
| `Record_representation` | ✓ | `super_errors_multi/Iface_record_representation_mismatch` | Interface declares `@unboxed`; implementation is boxed. |
| `Unboxed_representation` | ✓ | `super_errors_multi/Iface_unboxed_variant_mismatch` | |
| `Immediate` | ✓ | `super_errors_multi/Iface_immediate_mismatch` | Interface adds `@immediate`; implementation manifests a non-immediate (`string`). |
| `Tag_name` | ✓ | `super_errors_multi/Iface_tag_name_mismatch` | |
| `Variant_representation` | ✓ | `super_errors_multi/Iface_variant_representation_mismatch` | |

---

## `compiler/frontend/bs_syntaxerr.ml`

FFI / attribute / experimental-feature errors. Source: [bs_syntaxerr.ml:27](../compiler/frontend/bs_syntaxerr.ml).

| Variant | Status | Fixture | Notes |
|---|---|---|---|
| `Unsupported_predicates` | ✓ | `bs_unsupported_predicates.res` | `@get({weird: true})` on object type field. |
| `Duplicated_bs_deriving` | ✓ | `duplicated_bs_deriving.res` | |
| `Conflict_attributes` | ✓ | `bs_conflict_attributes.res` | |
| `Expect_int_literal` | ✓ | `bs_expect_int_literal.res` | |
| `Expect_string_literal` | ✓ | `bs_expect_string_literal.res` | |
| `Expect_int_or_string_or_json_literal` | ✓ | `bs_expect_int_or_string_or_json_literal.res` | `@as(true)` on a wildcard external argument. |
| `Invalid_underscore_type_in_external` | ✓ | `bs_invalid_underscore_type_in_external.res` | `@obj external make: (~x: _) => _ = ""` — `_` at an optional-label position without `@as`. |
| `Invalid_bs_string_type` | ✓ | `bs_invalid_bs_string_type.res` | |
| `Invalid_bs_int_type` | ✓ | `bs_invalid_bs_int_type.res` | |
| `Invalid_bs_unwrap_type` | ✓ | `bs_invalid_bs_unwrap_type.res` | |
| `Conflict_ffi_attribute` | ✓ | `conflicting_ffi_attributes.res` | |
| `Illegal_attribute` | ✓ | `bs_illegal_attribute_scope.res` | |
| `Not_supported_directive_in_bs_return` | ✓ | `bs_not_supported_directive_in_bs_return.res` | |
| `Expect_opt_in_bs_return_to_opt` | ✓ | `bs_expect_opt_in_bs_return_to_opt.res` | |
| `Optional_in_uncurried_bs_attribute` | ✓ | `bs_optional_in_uncurried_bs_attribute.res` | `@this` function with optional argument. |
| `Bs_this_simple_pattern` | ✓ | `bs_this_simple_pattern.res` | `@this` with destructured self pattern. |
| `Experimental_feature_not_enabled` | ✓ | `let_unwrap_on_top_level_not_enabled.res` (and other let-unwrap variants) | Currently only `LetUnwrap` is checked. |
| `LetUnwrap_not_supported_in_position` | ✓ | `let_unwrap_on_top_level.res`, `let_unwrap_on_not_supported_variant.res` | |
| `Misplaced_label_syntax` | ✓ | `misplaced_label_syntax.res` | Labelled args passed via the operator-identifier syntax `(->)`, `(#=)`, `(##)`, e.g. `\"->"(x, ~b=...)`. |

---

## `compiler/ml/ast_untagged_variants.ml`

Untagged-variant validation errors. Source: [ast_untagged_variants.ml:52](../compiler/ml/ast_untagged_variants.ml).

### `untagged_error`

| Variant | Status | Fixture | Notes |
|---|---|---|---|
| `OnlyOneUnknown` | ✓ | `UntaggedOnlyOneUnknown.res` | Abstract payload alongside another payload-carrying case. |
| `AtMostOneObject` | ✓ | `UntaggedAtMostOneObject.res` | Two record payloads in the same untagged variant. |
| `AtMostOneInstance` | ✓ | `UntaggedAtMostOneInstance.res` | Two cases with the same JS instance type (`Date.t` in both). |
| `AtMostOneFunction` | ✓ | `UntaggedAtMostOneFunction.res` | Two function-typed payloads. |
| `AtMostOneString` | ✓ | `UntaggedNonUnary*.res` (some sub-cases) | |
| `AtMostOneNumber` | ✓ | `UntaggedAtMostOneNumber.res` | `int` and `float` payloads collide on the number runtime check. |
| `AtMostOneBigint` | ✓ | `UntaggedAtMostOneBigint.res` | Two bigint payloads. |
| `AtMostOneBoolean` | ✓ | `UntaggedAtMostOneBoolean.res` | Two boolean payloads. |
| `DuplicateLiteral` | ✓ | `UntaggedDuplicateLiteral.res` | `@as("x")` on two different constructors. |
| `ConstructorMoreThanOneArg` | ✓ | `UntaggedConstructorMoreThanOneArg.res` | `A(int, int)` payload in an untagged variant. |

### `error`

| Variant | Status | Fixture | Notes |
|---|---|---|---|
| `InvalidVariantAsAnnotation` | ✓ | `UntaggedInvalidVariantAsAnnotation.res` | `@as(foo)` with a non-`null` / non-`undefined` identifier payload. |
| `Duplicated_bs_as` | ✓ | `UntaggedDuplicatedBsAs.res` | Two `@as("...")` attributes on the same constructor. |
| `InvalidVariantTagAnnotation` | ✓ | `UntaggedInvalidVariantTagAnnotation.res` | `@tag(123)` (non-string payload). |
| `InvalidUntaggedVariantDefinition` | ✓ | `UntaggedUnknown.res`, `UntaggedNonUnary*.res`, `UntaggedTupleAndArray.res`, `UntaggedImplIntf.res`, etc. | |
| `TagFieldNameConflict` | ✓ | `UntaggedTagFieldNameConflict.res` | `@tag("kind")` plus inline record field named `kind` on a constructor. |

---

## `compiler/depends/bs_exception.ml`

Build / dependency errors. Mostly need the `rescript build` runtime to fire — not reachable from raw `bsc`. Source: [bs_exception.ml:25](../compiler/depends/bs_exception.ml).

| Variant | Status | Fixture | Notes |
|---|---|---|---|
| `Cmj_not_found` | ☐ (needs build harness) | — | Missing `.cmj` from a dependent module. Reachable from `rescript build` but not from raw `bsc`. |
| `Js_not_found` | ✓ | implicitly — bypassed via `-bs-cmi-only` in `super_errors_multi` runner. Not a fixture, but the harness commit documents the workaround. | |
| `Bs_cyclic_depends` | ☐ (needs build harness) | — | Cycle across compilation units; the dependency graph that detects this is owned by `rewatch` / `bsb`, not raw `bsc`. |
| `Bs_duplicated_module` | ☐ (needs build harness) | — | Same module name in two source paths under a single package. |
| `Bs_duplicate_exports` | ☐ (needs build harness) | — | Same export emitted twice across compilation units. |
| `Bs_package_not_found` | ☐ (needs build harness) | — | `rescript.json`-referenced package not resolvable. |
| `Bs_main_not_exist` | ☐ (needs build harness) | — | `rescript.json` `main` entry missing. |
| `Bs_invalid_path` | ☐ (needs build harness) | — | `-I` / source path with invalid form. |
| `Missing_ml_dependency` | ☐ (needs build harness) | — | Compile-time missing dependency from a `.cmj` lookup table. |
| `Dependency_script_module_dependent_not` | ☐ (needs build harness) | — | `js_name_of_module_id.cppo.ml:122`. **Reachable** when a dependent module is in script mode (`Package_script`) but the current module is in package mode (`Package_found _`). Legacy script-vs-package interaction; needs `rescript.json` harness. |

---

## `compiler/ml/env.ml`

Environment / `.cmi`-consistency errors. Source: [env.ml:57](../compiler/ml/env.ml).

| Variant | Status | Fixture | Notes |
|---|---|---|---|
| `Illegal_renaming` | ☐ (needs build harness) | — | Triggered when a `.cmi` filename and the module name inside it disagree. Reachable via `rescript.json` setups that rename the produced artefact, but not from a single-process `bsc` invocation that always writes `Module.cmi` to match the source. |
| `Inconsistent_import` | ☐ (needs build harness) | — | Triggered when two `.cmi` files transitively imported by the same unit declare different CRCs for the same type. Needs an artificially-mutated build state across multiple compile invocations. |
| `Missing_module` | ☐ (needs build harness) | — | `.cmi` referenced but absent from `-I` paths at compile time. The `super_errors_multi` runner pre-compiles every fixture file via `-bs-read-cmi`, so it never reaches this code path. |
| `Illegal_value_name` | ✓ | `illegal_value_name.res` | Escaped identifier reaching `check_value_name` during definition (`let \"->" = 1`); the parser does not reject `\"->"`. |

---

## `compiler/ml/cmi_format.ml`

`.cmi` file format errors. Need binary-level manipulation to trigger
(write a non-`.cmi` file in place of one and reference it; downgrade
compiler version; truncate the file). Out of scope for the
`super_errors{,_multi}` harnesses, which only invoke `bsc` on
hand-written `.res` / `.resi` sources.

| Variant | Status | Fixture | Notes |
|---|---|---|---|
| `Not_an_interface` | ☐ (needs binary harness) | — | Pass an arbitrary file as `.cmi`. |
| `Wrong_version_interface` | ☐ (needs binary harness) | — | Mismatched compiler versions writing/reading. |
| `Corrupted_interface` | ☐ (needs binary harness) | — | Truncated or corrupted `.cmi`. |

---

## `compiler/core/cmd_ast_exception.ml`

PPX-runtime errors. Source: [cmd_ast_exception.ml:24](../compiler/core/cmd_ast_exception.ml).
Both require running `bsc` with `-ppx <executable>` and exercising the
external process boundary. Not reachable from the single-file or
multi-file harnesses, which never set `-ppx`.

| Variant | Status | Fixture | Notes |
|---|---|---|---|
| `CannotRun` | ☐ (needs PPX harness) | — | PPX binary fails to execute (missing or non-executable). |
| `WrongMagic` | ☐ (needs PPX harness) | — | PPX returns wrong AST magic number (e.g. PPX built against a different compiler ABI). |

---

## Single-variant modules

| Module | Variant | Status | Fixture | Notes |
|---|---|---|---|---|
| `compiler/ml/translcore.ml` | `Unknown_builtin_primitive` | ✓ | `unknown_builtin_primitive.res` | |
| `compiler/ml/translmod.ml` | `Fragile_pattern_in_toplevel` | ✓ | `fragile_pattern_toplevel.res` | |
| `compiler/ml/transl_recmodule.ml` | `Circular_dependency` | ✓ | `recmodule_circular_dependency.res` | |
| `compiler/ml/rec_check.ml` | `Illegal_letrec_expr` | ✓ | `illegal_letrec_expr.res` | |
| `compiler/ml/syntaxerr.ml` | `Variable_in_scope` | ? (live, broken printer) | — | Reachable via `let f: type t. (t, 't) => t = …` (locally-abstract `t` collides with type variable `'t` during `varify_constructors`), but `Syntaxerr.error` has no registered pretty-printer, so it propagates as an uncaught `Fatal error: exception Syntaxerr.Error(_)`. Not removed because the variant is live; the fix should wire up a printer or convert the check into a regular typed diagnostic. |
| `compiler/ml/cmt_format.cppo.ml` | `Not_a_typedtree` | ☐ (needs binary harness) | — | cmt_format.cppo.ml:147. Fires when a tool reads a `.cmt` file whose first block isn't a typed tree. Reachable in principle by pointing the analyzer at an arbitrary file with a `.cmt` extension; out of scope for the source-only fixture harnesses. |
| `compiler/ext/bsc_args.ml` | `Unknown` | ☐ (needs CLI harness) | — | bsc_args.ml:45. Reachable trivially via `bsc --bogus`, but the `super_errors{,_multi}` runners only pass `bsc` a fixed flag list plus the source file — they can't exercise CLI-level errors. |
| `compiler/ext/bsc_args.ml` | `Missing` | ☐ (needs CLI harness) | — | Same as above: `bsc -o` (no following filename). Needs a harness that invokes `bsc` with crafted argv. |

---

## `compiler/frontend/ast_utf8_string.ml` (retained defensive family)

Source: [ast_utf8_string.ml:25](../compiler/frontend/ast_utf8_string.ml). Re-validation found these are source-unreachable for regular ReScript, but not completely dead: `transform_test` and the defensive string-transform path still raise them, and the OUnit unicode tests assert their offsets. Retained.

| Variant | Status |
|---|---|
| `Invalid_code_point` | ? (source-unreachable, retained defensive/test helper) |
| `Unterminated_backslash` | ? (source-unreachable, retained defensive/test helper) |
| `Invalid_hex_escape` | ? (source-unreachable, retained defensive/test helper) |
| `Invalid_unicode_escape` | ? (source-unreachable, retained defensive/test helper) |
| `Invalid_unicode_codepoint_escape` | ? (source-unreachable, retained defensive/test helper) |

## `compiler/frontend/ast_utf8_string_interp.ml` (retained test family)

Source: [ast_utf8_string_interp.ml:25](../compiler/frontend/ast_utf8_string_interp.ml).

`pos_error` is reached through `transform_test`, which is intentionally
used by OUnit tests. Modern ReScript backtick templates take the
`BackQuotes` branch of `transform_exp` and skip the interpolation parser,
so these are source-unreachable for regular ReScript, but not completely
dead. Retained.

| Variant | Status |
|---|---|
| `Invalid_code_point` | ? (source-unreachable, retained test helper) |
| `Unterminated_backslash` | ? (source-unreachable, retained test helper) |
| `Invalid_escape_code` | ? (source-unreachable, retained test helper) |
| `Invalid_hex_escape` | ? (source-unreachable, retained test helper) |
| `Invalid_unicode_escape` | ? (source-unreachable, retained test helper) |
| `Unterminated_variable` | ? (source-unreachable, retained test helper) |
| `Unmatched_paren` | ? (source-unreachable, retained test helper) |
| `Invalid_syntax_of_var` | ? (source-unreachable, retained test helper) |

---

## Removal audit notes

All variants that were confirmed completely dead in this pass are listed
in **Removed in `jono/remove-dead-errors`** above and no longer appear in
the module tables. Previously flagged entries that were only unreachable
from regular source, but still possible through stale build artifacts,
PPX/malformed ASTs, or test helper APIs, were retained and reclassified.

---

## Warnings

Warnings are declared in [`compiler/ext/warnings.ml`](../compiler/ext/warnings.ml).
The default warning set is `+a-4-9-20-41-50-102`
([`compiler/ext/bsc_warnings.ml`](../compiler/ext/bsc_warnings.ml)), so
warnings 4, 9, 20, 41, 50, and 102 are disabled by default; the rest are
enabled. Fixtures use `-w +A` (everything on) so default-disabled
warnings still fire.

Fixtures follow the naming convention `warning_<NN>_<description>.res`
so coverage gaps stay greppable.

### Removed warnings

The warning constructors listed in **Removed in `jono/remove-dead-errors`**
were deleted. Their numeric warning slots remain holes; no warning number
was reused.

### Live but no fixture yet

These warnings have `prerr_warning` raise sites in `compiler/` and are
reachable from regular ReScript code, but no `super_errors` fixture
currently exercises them.

| Number | Variant | Trigger |
|---|---|---|
| 5 | `Partial_application` | `typecore.ml:2049`, `:3980` — fires from `check_application_result` and a guarded branch in the `ignore` special case. The 3980 branch needs `not total_app`, which would require `ignore(arg, ...)` partial application — syntactically non-sensical. The 2049 site fires via a delayed check whose only path is hard to trigger from plain source. Status: live raise sites but I couldn't construct a reproduction; may be effectively dead. |
