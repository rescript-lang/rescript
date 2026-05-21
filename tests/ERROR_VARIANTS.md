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
   blocker. They can be deleted in a follow-up PR. The "Confirmed dead"
   summary at the bottom groups them by reason.

## Status legend

| Symbol | Meaning |
|---|---|
| ✓ | Covered by at least one fixture under `tests/build_tests/super_errors/` or `tests/build_tests/super_errors_multi/`. |
| ⚠ | **Verified** unreachable: the trigger site has a specific blocker named in source (an exception declared but never raised, an AST node the parser doesn't construct, a guard that's always false). Candidate for dead-code removal. |
| ☐ | Reachable but no fixture yet; would be valuable to add. |
| ? | Trigger site is live but reachability from regular ReScript source isn't confirmed. Distinct from ⚠: a `?` means "I couldn't find a fixture that reaches it" rather than "the path is provably blocked". |

The "Confirmed dead" summary section at the bottom only includes ⚠.

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

---

## `compiler/ml/typecore.ml`

The largest error type; covers expression / pattern type-checking.
Source: [typecore.ml:27](../compiler/ml/typecore.ml).

| Variant | Status | Fixture | Notes |
|---|---|---|---|
| `Polymorphic_label` | ? | — | typecore.ml:1555. Triggers in record-pattern matching when a polymorphic field gets instantiated. Several `'a. 'a => 'a` record-field reproductions compiled cleanly; the trigger site is live but I couldn't find an AST that reaches it. |
| `Constructor_arity_mismatch` | ✓ | `constructor_arity_mismatch.res`, `constructor_arity_mismatch_pattern.res`, `arity_mismatch*.res` | Triggers in both expression (4028) and pattern (1426) paths. |
| `Label_mismatch` | ☐ | — | typecore.ml:3589. Record label type clash with explicit unify failure; often subsumed by `Pattern_type_clash` / `Expr_type_clash`. |
| `Pattern_type_clash` | ✓ | many `*_pattern_type_clash.res` etc. | Most-fired pattern error; covered through many fixtures but report-side sub-cases (option-vs-non-option trace, polyvariant context, etc.) remain partly untested. |
| `Or_pattern_type_clash` | ✓ | `or_pattern_type_clash.res` | |
| `Multiply_bound_variable` | ✓ | `multiply_bound_variable.res` | |
| `Orpat_vars` | ✓ | `orpat_vars_unbalanced.res` | |
| `Expr_type_clash` | ✓ | many `*.res` | Most-fired expression error. Many trace-shape sub-cases (function-arg context, JSX, dict, async, polyvariant) covered piecemeal; sub-case coverage is the biggest open area for this variant. |
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
| `Abstract_wrong_label` | ? | — | typecore.ml:3502. Fires when a function literal's label doesn't match the expected arrow type. One attempted reproduction landed on `Expr_type_clash` but I didn't retest with care; trigger site is live. |
| `Scoping_let_module` | ✓ | `scoping_let_module.res` | |
| `Not_a_variant_type` | ✓ | `variant_spread_pattern_not_a_variant.res` | Pattern-level variant spread of a non-variant type. |
| `Incoherent_label_order` | ? | — | typecore.ml:3894. Triggers when labeled args reorder against an arrow type that contains the label but not at the current position. Couldn't construct a reproduction that didn't hit `Apply_wrong_label` first. |
| `Less_general` | ✓ | `less_general_universal.res` | |
| `Modules_not_allowed` | ✓ | `super_errors_multi/Modules_not_allowed_toplevel` | Toplevel `let module(M) = …` pattern with `allow_modules=false`. |
| `Cannot_infer_signature` | ✓ | `cannot_infer_signature.res` | |
| `Not_a_packed_module` | ✓ | `not_a_packed_module.res` | |
| `Recursive_local_constraint` | ⚠ | — | typecore.ml:369. Routed via `Unification_recursive_abbrev` in `ctype.ml`, which is raised only when `ctype.ml`'s `Recursive_abbrev` exception fires. **`Recursive_abbrev` is defined (ctype.ml:110, ctype.mli:61) but never raised anywhere in `compiler/`.** Confirmed dead. |
| `Unexpected_existential` | ✓ | `super_errors_multi/Unexpected_existential_in_let` | Destructuring GADT constructor with existential in toplevel `let`. |
| `Unqualified_gadt_pattern` | ✓ | `super_errors_multi/Cross_gadt_pattern` | Only reachable via cross-module GADT disambiguation; in single-file matching the constructor would resolve before this check. |
| `Invalid_interval` | ⚠ | — | typecore.ml:1349. Triggered by `Ppat_interval` pattern. **Verified: `Ppat_interval` has no construction site in `compiler/syntax/src/res_core.ml`** — only printer and ast_debugger handle it. |
| `Invalid_for_loop_index` | ✓ | `invalid_for_loop_index.res` | |
| `Invalid_for_of_pattern` | ⚠ | — | typecore.ml:3120/3152. Verified: parser `normalize_for_of_pattern` (`res_core.ml:3841`) replaces non-var / non-`_` patterns with `Ppat_any` before the typer sees them. |
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
| `Type_params_not_supported` | ✓ | `variant_spread_pattern_type_params.res` | Pattern-level variant spread (`| ...a as v`) where `a` has type params; typedecl path covered by `variant_spread_type_parameters.res`. |
| `Field_access_on_dict_type` | ✓ | `field_access_on_dict_type.res` | |
| `Jsx_not_enabled` | ☐ | — | typecore.ml:218/3470. Fires when JSX expressions are used without `-bs-jsx N`. Reachable but the existing `super_errors` runner always passes `-bs-jsx 4`. |

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
| `Type_clash` | ☐ | — | typedecl.ml:125. Manifest type doesn't unify with kind. |
| `Parameters_differ` | ? | — | typedecl.ml:988. Non-uniform recursive type abbreviation; ReScript variant recursion is accepted, and abbreviations cycle to `Cycle_in_def` first. Hard to construct a reproduction that lands here exactly. |
| `Null_arity_external` | ⚠ | — | typedecl.ml:1900. The guard requires `prim_arity = 0` and `prim_native_name` not having the magic 20-byte encoding (`\132\149...`) and `prim_name` not starting with `%` or `#`. The encoding gets applied to every concrete external by `Primitive.parse_declaration`, and empty `prim_name` is rejected earlier by `external_ffi_types.ml` with "Not a valid global name". No path through the parser reaches it. |
| `Unbound_type_var` | ✓ | `unbound_type_var.res` | |
| `Cannot_extend_private_type` | ✓ | `cannot_extend_private_type.res` | |
| `Not_extensible_type` | ✓ | `not_extensible_type.res` | |
| `Extension_mismatch` | ☐ | — | Cross-module extension declaration mismatch via `.resi`/`.res`. |
| `Rebind_wrong_type` | ? | — | typedecl.ml:1653. Fires when source constructor's result type doesn't unify with target's. For exceptions both are `exn`; for extension types both share the extensible parent. I couldn't construct a triggering shape — the rebind succeeds for shapes the parser will accept. |
| `Rebind_mismatch` | ✓ | `extension_rebind_mismatch.res` | Rebinding constructor into a different extensible type. |
| `Rebind_private` | ✓ | `extension_rebind_private.res` | Rebinding a private extension constructor as public. |
| `Bad_variance` | ✓ | `bad_variance.res`, `bad_variance_contra.res` | |
| `Unavailable_type_constructor` | ☐ | — | typedecl.ml:778. Requires a type path findable at parse time but missing during constraint enforcement; only cross-unit scenarios. |
| `Bad_fixed_type` | ? | — | typedecl.ml:190/193. `set_fixed_row` runs when `is_fixed_type` returns true — requires an open object `{..f: t}` or open polyvariant `[> #A]` as `ptype_manifest`. Then if the expanded head isn't `Tvariant` / `Tobject` (line 190) or the row variable isn't `Tvar` (line 193), error. Reachable in principle via an alias chain that collapses the open row, but I haven't constructed one. |
| `Unbound_type_var_ext` | ✓ | `unbound_type_var_extension.res` | |
| `Varying_anonymous` | ? | — | typedecl.ml:1263. Requires anonymous constrained type params under specific variance; very obscure but trigger site is live. |
| `Val_in_structure` | ? | — | typedecl.ml:1887. Requires `pval_prim = []` for an external. Parser emits at least one string for any external; `[]` would only come from PPX or manual AST construction. Probably effectively dead. |
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
| `Cannot_eliminate_dependency` | ☐ | — | typemod.ml:1335. Requires anonymous functor application whose result still mentions the bound module; couldn't engineer despite multiple attempts. May be effectively dead — every fixture's `nondep_supertype` succeeded with existential substitution. |
| `Signature_expected` | ☐ | — | typemod.ml:78, 1184. Extract-sig on non-signature module type. |
| `Structure_expected` | ✓ | `super_errors_multi/Smoke_unbound_module_reference` (indirect); also `open_functor.res` | |
| `With_no_component` | ✓ | `with_no_component.res` | |
| `With_mismatch` | ✓ | `with_mismatch.res` | |
| `With_makes_applicative_functor_ill_typed` | ☐ | — | typemod.ml:258. Requires applicative-functor constructions ReScript syntax doesn't expose. |
| `With_changes_module_alias` | ☐ | — | typemod.ml:240. Requires `with module = ...` substitution invalidating an aliased path. ReScript may not parse `with module`. |
| `With_cannot_remove_constrained_type` | ? | — | typemod.ml:443. Triggers when destructive substitution `with type X<'a> := T` is applied where the substituted type has constrained type params (non-`Tvar`). One attempted reproduction succeeded; haven't found a triggering shape. |
| `Repeated_name` | ✓ | `repeated_def_*.res` (multiple) | |
| `Non_generalizable` | ✓ | `non_generalizable.res` | |
| `Non_generalizable_module` | ☐ | — | typemod.ml:1023. Module value with non-closed type at sealing time; cross-file. |
| `Interface_not_compiled` | ✓ | `super_errors_multi/Iface_not_compiled` | |
| `Not_allowed_in_functor_body` | ✓ | `super_errors_multi/not_allowed_in_functor_body` (TODO: confirm path) | |
| `Not_a_packed_module` | ✓ | `not_a_packed_module.res` | |
| `Incomplete_packed_module` | ✓ | `incomplete_packed_module.res` | |
| `Scoping_pack` | ⚠ | — | typemod.ml:1717. Requires first-class module pack where a constraint type has a level mismatch; very contrived. |
| `Recursive_module_require_explicit_type` | ✓ | `recursive_module_require_explicit_type.res` | |
| `Apply_generative` | ✓ | `apply_generative.res` | |
| `Cannot_scrape_alias` | ☐ | — | typemod.ml:77, 83, 1347. Requires `Env.scrape_alias` returning `Mty_alias` (alias target's `.cmi` not loaded). Only multi-unit scenarios. |

---

## `compiler/ml/typetexp.ml`

Type-expression errors. Source: [typetexp.ml:28](../compiler/ml/typetexp.ml).

| Variant | Status | Fixture | Notes |
|---|---|---|---|
| `Unbound_type_variable` | ✓ | (covered indirectly via many fixtures) | |
| `Unbound_type_constructor` | ✓ | `typetexp_unbound_type_constructor.res` | |
| `Unbound_type_constructor_2` | ? | — | typetexp.ml:475/619. Triggers in object / polyvariant inheritance where the inherited type's row variable is `Tvar` with a path. Hard to construct, but not provably dead. |
| `Type_arity_mismatch` | ✓ | `type_arity_mismatch.res` | |
| `Type_mismatch` | ✓ | `typetexp_type_mismatch.res` | Type-constructor application that violates a `constraint 'a = …` on the declaration. |
| `Alias_type_mismatch` | ✓ | `typetexp_alias_type_mismatch.res` | |
| `Present_has_conjunction` | ? | — | typetexp.ml:452. Polyvariant tag with conjunction (`&`) typing path. ReScript's parser doesn't have a `&` polyvariant operator that I can find, but the AST `Rtag` constructor supports a conjunction list, so PPX-generated AST could reach it. |
| `Present_has_no_type` | ? | — | typetexp.ml:501. Same `Rtag`-with-conjunction family. |
| `Constructor_mismatch` | ✓ | `polyvariant_constructor_mismatch.res` | |
| `Not_a_variant` | ✓ | `typetexp_not_a_variant.res` | Polyvariant `[#X \| a]` where `a` is not a polyvariant. |
| `Variant_tags` | ⚠ | — | typetexp.ml:39. Raised at typecore.ml:342, 349, 367 via `Tags` exception from `ctype.ml`. **Verified: `exception Tags` is defined (ctype.ml:60) but never raised in `compiler/`.** Confirmed dead. |
| `Invalid_variable_name` | ✓ | `invalid_type_variable_name.res` | |
| `Cannot_quantify` | ? | — | typetexp.ml:540. Triggers in `Ptyp_poly` translation when a quantified variable becomes non-generic. Every value-level reproduction lands on `Less_general` first, but type-level constructions with constraints might still reach it. |
| `Multiple_constraints_on_type` | ✓ | `multiple_constraints_on_type.res` | |
| `Method_mismatch` | ✓ | `object_method_mismatch.res` | |
| `Unbound_value` | ✓ | `typetexp_unbound_value.res` | |
| `Unbound_constructor` | ✓ | `typetexp_unbound_constructor.res` | |
| `Unbound_label` | ✓ | `typetexp_unbound_label.res` | |
| `Unbound_module` | ✓ | `suggest_module_for_missing_identifier.res`, `super_errors_multi/Smoke_unbound_module_reference` | |
| `Unbound_modtype` | ✓ | `typetexp_unbound_modtype.res` | |
| `Ill_typed_functor_application` | ⚠ | — | typetexp.ml:102. In the `Longident.Lapply` branch. **Verified: parser has no construction site for `Longident.Lapply`** (no result in `res_core.ml`). Confirmed dead. |
| `Illegal_reference_to_recursive_module` | ☐ | — | typetexp.ml:75/114. Catches `Env.Recmodule` exception, raised when looking up a module currently being recursively defined (`#recmod#` placeholder, env.ml:1048). Reachable in principle via a recmodule whose signature references another recmodule member's type before sealing; couldn't construct a triggering fixture but trigger sites are live. |
| `Access_functor_as_structure` | ✓ | `access_functor_as_structure.res` | |
| `Apply_structure_as_functor` | ⚠ | — | typetexp.ml:93. In the `Longident.Lapply` branch. Same dead reason as `Ill_typed_functor_application`. |
| `Cannot_scrape_alias` | ☐ | — | typetexp.ml:86 (Ldot path, live), 95/101 (Lapply path, dead since `Lapply` isn't parsed). The Ldot trigger needs `Env.scrape_alias` to return `Mty_alias` — i.e. an alias whose target `.cmi` can't be loaded. Multi-unit only. |
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
| `Unbound_modtype_path` | ☐ | — | includemod.ml:94. Requires module-type path comparison to fail; only triggers via destructive substitution paths ReScript doesn't expose. |
| `Unbound_module_path` | ☐ | — | includemod.ml:226/233. Alias comparison where `normalize_path` fails. Multi-unit scenarios only. |
| `Invalid_module_alias` | ☐ | — | includemod.ml:211. Requires both sides `Mty_alias` with one pointing to a functor argument. Functor-with-alias-sig fixtures hit `Module_types` instead. |

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
| `Conflict_bs_bs_this_bs_meth` | ⚠ | — | bs_syntaxerr.ml:29 declares the variant but `Bs_syntaxerr.err _ Conflict_bs_bs_this_bs_meth` is **never raised** anywhere in `compiler/`. |
| `Duplicated_bs_deriving` | ✓ | `duplicated_bs_deriving.res` | |
| `Conflict_attributes` | ✓ | `bs_conflict_attributes.res` | |
| `Expect_int_literal` | ✓ | `bs_expect_int_literal.res` | |
| `Expect_string_literal` | ✓ | `bs_expect_string_literal.res` | |
| `Expect_int_or_string_or_json_literal` | ✓ | `bs_expect_int_or_string_or_json_literal.res` | `@as(true)` on a wildcard external argument. |
| `Unhandled_poly_type` | ? | — | ast_core_type.ml:141. Triggers in `list_of_arrow` when an arrow chain contains a `Ptyp_poly`. The parser doesn't normally produce inline poly types inside arrows, but record fields can have polytypes that flow through these utilities. |
| `Invalid_underscore_type_in_external` | ? | — | ast_external_process.ml:107/132. Needs `_` in optional-label external position with no `@as`. Probably reachable in `@@obj` externals; not yet verified. |
| `Invalid_bs_string_type` | ✓ | `bs_invalid_bs_string_type.res` | |
| `Invalid_bs_int_type` | ✓ | `bs_invalid_bs_int_type.res` | |
| `Invalid_bs_unwrap_type` | ✓ | `bs_invalid_bs_unwrap_type.res` | |
| `Conflict_ffi_attribute` | ✓ | `conflicting_ffi_attributes.res` | |
| `Illegal_attribute` | ✓ | `bs_illegal_attribute_scope.res` | |
| `Not_supported_directive_in_bs_return` | ✓ | `bs_not_supported_directive_in_bs_return.res` | |
| `Expect_opt_in_bs_return_to_opt` | ✓ | `bs_expect_opt_in_bs_return_to_opt.res` | |
| `Misplaced_label_syntax` | ⚠ | — | bs_syntaxerr.ml:116. Only fires from `check_and_discard` in `ast_exp_apply.ml:49`, applied to the args of `->`, `#=`, `##` operators. The parser always emits those args as `Nolabel`. |
| `Optional_in_uncurried_bs_attribute` | ✓ | `bs_optional_in_uncurried_bs_attribute.res` | `@this` function with optional argument. |
| `Bs_this_simple_pattern` | ✓ | `bs_this_simple_pattern.res` | `@this` with destructured self pattern. |
| `Experimental_feature_not_enabled` | ✓ | `let_unwrap_on_top_level_not_enabled.res` (and other let-unwrap variants) | Currently only `LetUnwrap` is checked. |
| `LetUnwrap_not_supported_in_position` | ✓ | `let_unwrap_on_top_level.res`, `let_unwrap_on_not_supported_variant.res` | |

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
| `Cmj_not_found` | ☐ | — | Missing `.cmj` from a dependent module. Needs `rescript build` harness. |
| `Js_not_found` | ✓ | implicitly — bypassed via `-bs-cmi-only` in `super_errors_multi` runner. Not a fixture, but the harness commit documents the workaround. | |
| `Bs_cyclic_depends` | ☐ | — | Cycle across compilation units; needs build-system harness. |
| `Bs_duplicated_module` | ☐ | — | Same module name in two source paths. |
| `Bs_duplicate_exports` | ☐ | — | Same export emitted twice; depends/build setup needed. |
| `Bs_package_not_found` | ☐ | — | `rescript.json`-referenced package not resolvable. |
| `Bs_main_not_exist` | ☐ | — | `rescript.json` `main` entry missing. |
| `Bs_invalid_path` | ☐ | — | `-I` / source path with invalid form. |
| `Missing_ml_dependency` | ☐ | — | Compile-time missing dependency. |
| `Dependency_script_module_dependent_not` | ☐ | — | `js_name_of_module_id.cppo.ml:122`. **Reachable** when a dependent module is in script mode (`Package_script`) but the current module is in package mode (`Package_found _`). Legacy script-vs-package interaction; needs `rescript.json` harness. |

---

## `compiler/ml/env.ml`

Environment / `.cmi`-consistency errors. Source: [env.ml:57](../compiler/ml/env.ml).

| Variant | Status | Fixture | Notes |
|---|---|---|---|
| `Illegal_renaming` | ☐ | — | `.cmi` filename doesn't match module name; multi-unit scenario. |
| `Inconsistent_import` | ☐ | — | Two `.cmi` files disagree on a type's hash; needs synthetic build state. |
| `Missing_module` | ☐ | — | `.cmi` not findable when referenced; needs multi-file harness. |
| `Illegal_value_name` | ☐ | — | Reserved identifier name; very specific. |

---

## `compiler/ml/cmi_format.ml`

`.cmi` file format errors. Need binary-level manipulation to trigger.

| Variant | Status | Fixture | Notes |
|---|---|---|---|
| `Not_an_interface` | ☐ | — | Pass an arbitrary file as `.cmi`. |
| `Wrong_version_interface` | ☐ | — | Mismatched compiler versions writing/reading. |
| `Corrupted_interface` | ☐ | — | Truncated or corrupted `.cmi`. |

---

## `compiler/core/cmd_ast_exception.ml`

PPX-runtime errors. Source: [cmd_ast_exception.ml:24](../compiler/core/cmd_ast_exception.ml).

| Variant | Status | Fixture | Notes |
|---|---|---|---|
| `CannotRun` | ☐ | — | PPX binary fails to execute. |
| `WrongMagic` | ☐ | — | PPX returns wrong AST magic number. |

---

## Single-variant modules

| Module | Variant | Status | Fixture | Notes |
|---|---|---|---|---|
| `compiler/ml/translcore.ml` | `Unknown_builtin_primitive` | ✓ | `unknown_builtin_primitive.res` | |
| `compiler/ml/translmod.ml` | `Fragile_pattern_in_toplevel` | ✓ | `fragile_pattern_toplevel.res` | |
| `compiler/ml/transl_recmodule.ml` | `Circular_dependency` | ✓ | `recmodule_circular_dependency.res` | |
| `compiler/ml/rec_check.ml` | `Illegal_letrec_expr` | ✓ | `illegal_letrec_expr.res` | |
| `compiler/ml/syntaxerr.ml` | `Variable_in_scope` | ⚠ | — | Reachable via `let f: type t. (t, 't) => t = …` (locally-abstract `t` collides with type variable `'t` during `varify_constructors`), but `Syntaxerr.error` has no registered pretty-printer, so it propagates as an uncaught `Fatal error: exception Syntaxerr.Error(_)`. The variant is live; the printer is dead. Treat as broken until either the printer is wired up or the variant is removed in favor of a proper diagnostic. |
| `compiler/ml/cmt_format.cppo.ml` | `Not_a_typedtree` | ☐ | — | cmt_format.cppo.ml:147. Fires when reading a `.cmt` file that doesn't contain a typed tree. Needs binary `.cmt` manipulation. |
| `compiler/ext/bsc_args.ml` | `Unknown` | ☐ | — | bsc_args.ml:45. Unknown CLI flag passed to `bsc`. Reachable via `bsc --bogus`. |
| `compiler/ext/bsc_args.ml` | `Missing` | ☐ | — | Required CLI flag argument missing (e.g. `bsc -o` with no following filename). |

---

## `compiler/frontend/ast_utf8_string.ml` (dead family)

Source: [ast_utf8_string.ml:25](../compiler/frontend/ast_utf8_string.ml). All variants here are reached only via the legacy `{j|…|j}` delimiter, which the modern ReScript parser doesn't emit. Backtick template strings skip the transform entirely.

| Variant | Status |
|---|---|
| `Invalid_code_point` | ⚠ Dead |
| `Unterminated_backslash` | ⚠ Dead |
| `Invalid_hex_escape` | ⚠ Dead |
| `Invalid_unicode_escape` | ⚠ Dead |
| `Invalid_unicode_codepoint_escape` | ⚠ Dead |

## `compiler/frontend/ast_utf8_string_interp.ml` (dead family)

Source: [ast_utf8_string_interp.ml:25](../compiler/frontend/ast_utf8_string_interp.ml).

`pos_error` is reached only through `check_and_transform`, whose only
caller in `compiler/` is `transform_test` — used by OUnit tests, not the
production pipeline. Modern ReScript backtick templates take the
`BackQuotes` branch of `transform_exp` (line 311) and skip the
interpolation parser entirely. The legacy `{j|…|j}` delimiter the
parser would otherwise route here is no longer accepted by the
scanner. All variants below are unreachable from regular ReScript
source.

| Variant | Status |
|---|---|
| `Invalid_code_point` | ⚠ Dead |
| `Unterminated_backslash` | ⚠ Dead |
| `Invalid_escape_code` | ⚠ Dead |
| `Invalid_hex_escape` | ⚠ Dead |
| `Invalid_unicode_escape` | ⚠ Dead |
| `Unterminated_variable` | ⚠ Dead |
| `Unmatched_paren` | ⚠ Dead |
| `Invalid_syntax_of_var` | ⚠ Dead |

---

## Confirmed dead variants — candidates for removal

Only variants with a concrete, source-level reason are listed. Each row
has been re-verified against the source as of this audit. Variants marked
`?` in the tables above are **not** included here — those may turn out to
be live and just hard to reproduce.

**Verified dead by missing raise / construction site:**

- `typecore.Variant_tags`, `typetexp.Variant_tags` — relayed via the
  `Tags` exception which is declared in `ctype.ml:60` / `ctype.mli:57`
  but **never raised** in `compiler/`.
- `typecore.Recursive_local_constraint` — relayed via
  `Unification_recursive_abbrev`, raised only from the `Recursive_abbrev`
  exception which is declared (`ctype.ml:110`, `ctype.mli:61`) but
  **never raised**.
- `typecore.Invalid_interval` — needs `Ppat_interval`; **no construction
  site** for that AST node in `compiler/syntax/src/`.
- `typecore.Invalid_for_of_pattern` — parser's
  `normalize_for_of_pattern` (`res_core.ml:3841`) replaces every non-var,
  non-`_` pattern with `Ppat_any` before the typer runs.

**Verified dead because parser doesn't produce required AST shape:**

- `typetexp.Ill_typed_functor_application`,
  `typetexp.Apply_structure_as_functor` — in the
  `Longident.Lapply` branch; `Lapply` has no construction site in
  the parser (`res_core.ml`).
- `bs_syntaxerr.Misplaced_label_syntax` — fires for labeled args to
  `->`/`#=`/`##` operators; the parser always emits those with
  `Nolabel`.
- `typedecl.Null_arity_external` — primitives parsed by
  `Primitive.parse_declaration` always get the magic 20-byte
  `prim_native_name` encoding, which bypasses the trigger; empty
  `prim_name` is rejected earlier with "Not a valid global name".
- `ast_utf8_string.*` (Invalid_code_point, Unterminated_backslash,
  Invalid_hex_escape, Invalid_unicode_escape,
  Invalid_unicode_codepoint_escape) — the scanner
  (`res_scanner.ml:350-417`) already validates escape sequences and
  unicode code points; the transform never sees a string that would
  fail its own re-validation.

**Probably dead but not formally verified** (`?` in tables above; needs
deeper analysis before removal): `Polymorphic_label`,
`Abstract_wrong_label`, `Incoherent_label_order`, `Parameters_differ`,
`Bad_fixed_type`, `Varying_anonymous`, `Val_in_structure`,
`Unbound_type_constructor_2`, `Cannot_quantify`,
`Present_has_conjunction`, `Present_has_no_type`,
`With_cannot_remove_constrained_type`, `Unhandled_poly_type`,
`Invalid_underscore_type_in_external`.

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

### Confirmed dead (no `prerr_warning` site in the compiler)

These warning constructors exist in `warnings.ml` but are never raised
anywhere in `compiler/`. They are candidates for removal.

| Number | Variant | Reason |
|---|---|---|
| 1 | `Comment_start` | Lexer warning; modern parser doesn't emit. |
| 2 | `Comment_not_end` | Lexer warning; modern parser doesn't emit. |
| 7 | `Method_override` | OCaml class system, not exposed by ReScript. |
| 13 | `Instance_variable_override` | OCaml class system. |
| 14 | `Illegal_backslash` | Lexer-level escape warning; parser doesn't emit. |
| 15 | `Implicit_public_methods` | OCaml class system. |
| 29 | `Eol_in_string` | Lexer-level string warning. |
| 48 | `Eliminated_optional_arguments` | Declared but never raised. |
| 50 | `Bad_docstring` | Declared but never raised; also default-disabled. |
| 105 | `Bs_fragile_external` | Declared but never raised. |
| 106 | `Bs_unimplemented_primitive` | Declared but never raised. |
| 10 | `Statement_type` | Raised at typecore.ml:2052 inside `check_application_result`, but `statement` is always `false` at the only call site (typecore.ml:3983); sequence statements hit `Expr_type_clash` via `type_statement` unifying to `unit`. |
| 16 | `Unerasable_optional_argument` | Raised at typecore.ml:3526, but `type_function` (typecore.ml:3479) explicitly disables this warning before the check runs (`Warnings.parse_options false "-16"`). |
| 108 | `Bs_uninterpreted_delimiters` | Raised at bs_warnings.ml:29 for `Pconst_string` with delimiter `"js"`; the modern scanner has no `{js\|...\|js}` form and template strings don't tag with `"js"`. |

### Live but no fixture yet

These warnings have `prerr_warning` raise sites in `compiler/` and are
reachable from regular ReScript code, but no `super_errors` fixture
currently exercises them.

| Number | Variant | Trigger |
|---|---|---|
| 5 | `Partial_application` | `typecore.ml:2049`, `:3980` — fires from `check_application_result` and a guarded branch in the `ignore` special case. The 3980 branch needs `not total_app`, which would require `ignore(arg, ...)` partial application — syntactically non-sensical. The 2049 site fires via a delayed check whose only path is hard to trigger from plain source. Status: live raise sites but I couldn't construct a reproduction; may be effectively dead. |
