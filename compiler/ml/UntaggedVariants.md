# Untagged variants

An untagged variant is a variant type declared with `@unboxed` whose cases are
represented by their bare JavaScript values: a nullary case by a literal, a
case with a payload by the payload itself. No field records which case a value
belongs to, so a `switch` recognizes the case by testing the value at run
time, and the type checker limits the payload cases to kinds these tests tell
apart (rule 6). The user manual describes the feature in
[Untagged variants](https://rescript-lang.org/docs/manual/variant/#untagged-variants).

This document states the rules the compiler implements. Each rule names the
code that implements it and the tests that pin it. Tests named
`spec_test` are in
[`tests/tests/src/untagged_variants_spec_test.res`](../../tests/tests/src/untagged_variants_spec_test.res);
the committed `.mjs` beside it shows the checks emitted for each declaration.
Fixtures are in `tests/build_tests/super_errors/fixtures/` unless another
directory is named.

## Rules

### 1. Declaration

`@unboxed` and `@tag(...)` on a variant type are copied onto every
constructor. `@unboxed` on a single constructor of an ordinary variant makes
that constructor untagged and leaves the others tagged:

| Declaration | Value | JavaScript |
| --- | --- | --- |
| `type mixed = \| @unboxed MA(int) \| MB({x: int}) \| MC` | `MA(3)` | `3` |
| | `MB({x: 1})` | `{TAG: "MB", x: 1}` |
| | `MC` | `"MC"` |

The layout of a declaration is computed once, after its recursive group is in
the environment, and stored on the declaration.

- Implementation: `typedecl.ml` (`copy_tag_attr_from_decl` in
  `transl_declaration`; the layout pass in `transl_type_decl`),
  `variant_layout.ml:layout_from_type_variant`.
- Tests: `spec_test` rule 1.
- Shaped by #7320 (recursive groups), #8579 (layout stored once).

### 2. Representation

A nullary case is its `@as` literal, or the string of its name when it has no
`@as`. A case with a payload is the payload; an inline-record payload is a
plain object holding the fields. In an untagged variant, `@as` on a case with
a payload and `@tag` on the type do not change the representation.

| Declaration | Value | JavaScript |
| --- | --- | --- |
| `@unboxed type t = Named \| @as("s") Str \| @as(null) Null \| Payload({x: int})` | `Named` | `"Named"` |
| | `Str` | `"s"` |
| | `Null` | `null` |
| | `Payload({x: 1})` | `{x: 1}` |
| `@unboxed @tag("kind") type t = TI({x: int}) \| TS(string)` | `TI({x: 1})` | `{x: 1}` |
| `@unboxed type t = @as("x") AsX(string) \| AsY(int)` | `AsX("v")` | `"v"` |

- Implementation: `variant_runtime.ml:compute_matching_facts` (a nullary case
  without `@as` is its name), `translcore.ml:transl_exp0` (constructor
  application).
- Tests: `spec_test` rule 2.

### 3. `@as` literals

`@as` takes a string, an int, a float, a bigint, a boolean, `null` or
`undefined`. Any other payload is an error, and so is a second `@as` on one
constructor. An int literal must fit the compiler's native integer; it is
emitted with the 32-bit representation of `int` values, so `@as(4294967297)`
is the number `1`.

- Implementation: `ast_untagged_variants.ml:runtime_tag_of_parsetree`,
  `process_constructor_tag`.
- Tests: fixtures `UntaggedInvalidVariantAsAnnotation.res`
  ("A variant case annotation @as(...) must be a string, integer, float,
  bigint, boolean, null, or undefined."), `VariantAsIntegerOutOfRange.res`,
  `UntaggedDuplicatedBsAs.res`; `spec_test` rule 2;
  `tests/tests/src/unboxed_variant_fold_test.res` (`wide`).

### 4. Payload kinds

A case of an untagged variant has at most one payload. Its type is classified
into one kind:

| Payload type | Kind | Recognized by |
| --- | --- | --- |
| `string` | string | `typeof x === "string"` |
| `int`, `float` | number | `typeof x === "number"` |
| `bigint` | bigint | `typeof x === "bigint"` |
| `bool` | boolean | `typeof x === "boolean"` |
| a function type | function | `typeof x === "function"` |
| a record (not `@unboxed`), an inline record, `dict<_>` | object | `typeof x === "object"` |
| `array<_>`, a tuple | instance `Array` | `Array.isArray(x)` |
| a type in the list of rule 5 | instance `C` | `x instanceof C` |
| anything else: an abstract type, a type variable, a variant, `list<_>`, an `@unboxed` record | unknown | no check |

The payload type is classified after expanding abbreviations
(`type myDate = Date.t` is an instance case), except that the list of rule 5
is consulted first on the type as written. `@notUndefined` on an abstract type
leaves it unknown.

- Implementation: `variant_layout.ml:get_block_type_from_typ`,
  `get_block_type`; `ast_untagged_variants.ml:type_is_builtin_object`.
- Tests: `spec_test` rules 4 (`kinds`, `tuples`, `aliasDict`, `instances`,
  `unknown`); fixtures `UntaggedConstructorMoreThanOneArg.res`,
  `UntaggedConstructorAttributeMoreThanOneArg.res`, `UntaggedNonUnary1.res`,
  `UntaggedNonUnary2.res` ("Constructor A has more than one argument."),
  `UntaggedUnknown.res` (`list<float>` is unknown).
- Shaped by #6121, #6208, #6219, #6279, #6368, #6394, #6550, #6670.

### 5. Instance types

The instance cases come from a fixed list of type names in
`ast_untagged_variants.ml:type_to_instanceof_backed_obj`. The predefined
`promise<_>` and `array<_>` are matched by path; the others by the printed
path of the type:

| Type | Class |
| --- | --- |
| `promise<_>` | `Promise` |
| `array<_>` | `Array` (checked with `Array.isArray`) |
| `Date.t`, `RegExp.t`, `File.t`, `Blob.t`, `ArrayBuffer.t`, `DataView.t` | the class of the same name |
| `Set.t`, `Map.t`, `WeakSet.t`, `WeakMap.t` | the class of the same name |
| `Int8Array.t`, `Uint8Array.t`, `Uint8ClampedArray.t`, `Int16Array.t`, `Uint16Array.t`, `Int32Array.t`, `Uint32Array.t`, `Float32Array.t`, `Float64Array.t`, `BigInt64Array.t`, `BigUint64Array.t` | the class of the same name |

A type outside this list, such as a binding to a Web API class, is unknown
(rule 4). The typed-array types are abbreviations of one type,
`TypedArray.t<_>`, and are told apart only by the name as written.

- Implementation: `ast_untagged_variants.ml:type_to_instanceof_backed_obj`,
  `variant_runtime.ml:Instance`.
- Tests: `spec_test` rules 4 and 5; fixtures `UntaggedAtMostOneInstance.res`,
  `UntaggedTupleAndArray.res`.
- Shaped by #6296, #6383, #6396, #7788, #8009. Related: #7147.

### 6. Well-formedness

The type checker rejects an untagged variant, with
"This untagged variant definition is invalid: ..." and the reason below, when:

| Condition | Reason | Fixture |
| --- | --- | --- |
| two object cases | At most one case can be an object type. | `UntaggedAtMostOneObject.res` |
| two cases of one instance class (arrays and tuples share `Array`) | At most one case can be a C type; for `Array`, an array or tuple type. | `UntaggedAtMostOneInstance.res`, `UntaggedTupleAndArray.res` |
| two function cases | At most one case can be a function type. | `UntaggedAtMostOneFunction.res` |
| two string cases | At most one case can be a string type. | `UntaggedAtMostOneString.res` |
| two number cases (`int` and `float` count together) | At most one case can be a number type (int or float). | `UntaggedAtMostOneNumber.res` |
| two bigint cases | At most one case can be a bigint type. | `UntaggedAtMostOneBigint.res` |
| two boolean cases | At most one case can be a boolean type. | `UntaggedAtMostOneBoolean.res` |
| an unknown case beside another case with a payload | Case A has a payload that is not of one of the recognized shapes ... | `UntaggedOnlyOneUnknown.res`, `UntaggedUnknown.res` |
| a `bool` case when both `true` and `false` are literal cases | Case B can never match ... | `UntaggedUnreachableBoolean.res` |
| two nullary cases with the same literal | Duplicate literal x. | `UntaggedDuplicateLiteral.res` |
| two nullary cases whose literals are spelled differently but are the same JavaScript value | The literal 1.0 is the same JavaScript value as 1. | `untagged_literal_same_number.res`, `untagged_literal_same_int32.res`, `untagged_literal_same_bigint.res` |

Literals are compared per kind: string literals with each other (a case
without `@as` counts as its name), and all other literals with each other by
the JavaScript value they denote, as the `===` of a `switch` compares them.
An int is the 32-bit number it is emitted as (rule 3), so `1`, `1.0` and
`4294967297` are one number, and so are `0` and `-0.`; `1n` is a bigint and
differs from `1`, and `001n` is `1n`. Constant folding (rule 11) compares
literals the same way. The duplicate-literal check applies to every variant, tagged
or not; in a tagged variant it also compares the `@as` tags of the cases with
a payload.

A literal may equal a value of a payload case: `@as("a") A | S(string)`,
`@as(1) A | I(int)`, `@as(null) N | O({x: int})` and
`@as(true) A | B(bool)` are accepted. Rule 7 says which case such a value
belongs to.

- Implementation: `ast_untagged_variants.ml:check_invariant`,
  `variant_runtime.ml:literal_value_key`.
- Tests: the fixtures above; `tests/tests/src/unboxed_variant_fold_test.res`
  (a number and a bigint literal with the same digits); `TaggedDuplicateLiteral.res`,
  `multiple_tag_1.res`, `multiple_tag_2.res` (tagged variants).
- Shaped by #6128, #6136, #7441, #8777, #8953.

### 7. Literal cases are tested first

A `switch` with literal and payload arms first tests whether the value is one
of the literal cases, compares it with `===` against each literal arm, and
only otherwise tests the payload cases. A value equal to a literal therefore
belongs to the literal's case, even when it was built with a payload
constructor:

| Declaration | Value | Case |
| --- | --- | --- |
| `@as("a") LitA \| AnyString(string)` | `AnyString("a")` | `LitA` |
| `@as(1) LitOne \| AnyInt(int)` | `AnyInt(1)` | `LitOne` |
| `@as(true) LitTrue \| AnyBool(bool)` | `AnyBool(true)` | `LitTrue` |
| `@as(true) LitTrue \| AnyBool(bool)` | `AnyBool(false)` | `AnyBool` |
| `@as(null) LitNull \| AnyObject({x: int})` | `LitNull` | `LitNull` |

The "is a literal case" test is the smaller of two equivalent expressions: the
disjunction of `x === literal` over the literal cases, or the conjunction of
the negated payload checks (with a payload check replaced by the literal
disjunction where a literal overlaps it). A `switch` whose arms name no
literal case still tests the literals first when one of them would pass the
check of a payload arm, such as `@as("lit")` beside a `string` arm, so that
it reaches the default arm.

- Implementation: `lam_compile.ml:compile_switch`,
  `ast_untagged_variants.ml:Dynamic_checks.is_a_literal_case`.
- Tests: `spec_test` rule 7; `tests/tests/src/untagged_variants_heuristic.res`
  (choice of expression).
- Shaped by #6125, #7128, #7135.

### 8. Payload cases

The payload arms compile in one of two forms.

- **Two arms, or one arm and a default**: one `if` on the check of the arm
  declared first, with the other branch unchecked (rule 10). The object check is
  `typeof x === "object"`, followed by `x !== null` when the variant has an
  `@as(null)` case, and by `!(x instanceof C)` (`!Array.isArray(x)` for
  arrays) for every instance case of the variant.
- **Three or more arms**: an `if` chain of the instance checks, in the
  declaration order of the cases, then one `switch (typeof x)` over the other
  arms.

In both forms a value of an instance case never lands in the object case.
An `int` case is recognized by `typeof x === "number"` and so receives every
number, integral or not.

- Implementation: `lam_compile.ml:compile_untagged_cases`,
  `compile_general_cases`;
  `ast_untagged_variants.ml:Dynamic_checks.add_runtime_type_check`.
- Tests: `spec_test` rules 4, 5 and 8 (`instanceName` shows the chain in
  declaration order, `objectFirst` both forms); `tests/tests/src/UntaggedVariants.res`
  (`AllInstanceofTypes`, `MergeCases`).
- Shaped by #6103, #6383, #6792, #7144, #7149.

### 9. `null` and arrays are not objects

`typeof null` and `typeof []` are `"object"`, and so is the `typeof` of a
class instance. The object case never receives `null` when the variant has an
`@as(null)` case, and never receives a value of another instance case (array,
tuple or class): the two-arm form adds the checks of rule 8, and the
`switch (typeof x)` form is preceded by `x === null`, `Array.isArray(x)` and
`x instanceof C` guards that send those values to the default arm when their
own arm is absent.

| Declaration | `switch` arms | Value | Arm |
| --- | --- | --- | --- |
| `GArray(array<int>) \| GObject({x: int}) \| @as(null) GNull` | all | `[]` | `GArray` |
| | all | `null` | `GNull` |
| | `GObject(_)`, `_` | `[]`, `null` | `_` |

- Implementation: `lam_compile.ml:compile_untagged_cases` (`needs_null_guard`,
  `unmatched_instances`), `Dynamic_checks.add_runtime_type_check`.
- Tests: `spec_test` rule 9; `UntaggedVariants.res` (`ObjectAndNull`, `Json`);
  `tests/tests/src/untagged_default_arm_test.res`.
- Shaped by #6219, #7303, #8253, #8255, #8279, #8995.

### 10. Values outside every case

Recognition trusts the type. A value that belongs to no case is not rejected:

- in the two-arm form it lands in the unchecked branch: with
  `I(int) | S(string)`, `true` lands in `S`; with
  `TDate(Date.t) | TRegExp(RegExp.t)`, `{x: 1}` lands in `TRegExp`; with
  `PPromise(promise<int>) | PFloat(float)`, a thenable that is not a
  `Promise` lands in `PFloat`;
- in the `switch (typeof x)` form it matches no arm, and the `switch`
  completes without running any arm.

A value from another JavaScript realm fails every `instanceof` check and is
recognized the same way (by the semantics of `instanceof`; not tested).

- Implementation: `lam_compile.ml:compile_general_cases` (a complete match
  emits no check for its last clause).
- Tests: `spec_test` rule 10 (two-arm form); `kindName` in
  `untagged_variants_spec_test.mjs` shows the `switch` form.

### 11. Constant folding

A `switch` on a constant untagged value folds to the arm that the run-time
dispatch selects for the constant's JavaScript value, so `S("a")` matched
against `@as("a") A | S(string)` folds to `A`.

- Implementation: `lambda.ml:switch` (`untagged_action`, `literal_denotes_same`).
- Tests: `tests/tests/src/unboxed_variant_fold_test.res`.
- Shaped by #8631.

### 12. `option` of an untagged variant

`Some(v)` is emitted as `v` when the type of `v` cannot hold `undefined`, and
as `Primitive_option.some(v)` otherwise. An untagged variant cannot hold
`undefined` when no case is `@as(undefined)` and every payload type is
`dict<_>` or cannot hold `undefined` itself:

| Payload type | Holds `undefined` |
| --- | --- |
| `string`, `int`, `float`, `bool`, `bigint`, a function, a tuple, a record without `@unboxed` | no |
| an inline record with two or more fields | no |
| an `@unboxed` record, an inline record with one field | as the type of its field |
| an abstract type with `@notUndefined` (rule 13) | no |
| an abstract type without it, a type variable, `option<_>` | yes |
| a type whose declaration is not on the load path | yes |

An `@as(null)` case does not prevent unboxing; `Some(N)` is `null`.

| Declaration | `Some(v)` |
| --- | --- |
| `@as(null) WNull \| WString(string)` | `v` |
| `@as(undefined) WUndefined \| WUString(string)` | `Primitive_option.some(v)` |
| `@as("n") ONone \| OPaque(opaque)` | `Primitive_option.some(v)` |
| `BBlob(Blob.t) \| BString(string)` | `Primitive_option.some(v)` |
| `DDate(Date.t) \| DString(string)` | `v` |
| `RRecord(record) \| RString(string)` with `type record = {y: option<int>}` | `v` |
| `INone \| IRecord({x: option<int>})` | `Primitive_option.some(v)` |
| `@as("n") UONone \| UOption(unboxedOption)` with `@unboxed type unboxedOption = {x: option<int>}` | `Primitive_option.some(v)` |
| `@as("n") USNone \| UString(unboxedString)` with `@unboxed type unboxedString = {s: string}` | `v` |

The stdlib marks `Date.t`, `RegExp.t`, `Map.t`, `Set.t`, `WeakMap.t`,
`WeakSet.t`, `ArrayBuffer.t`, `DataView.t`, the typed arrays and other
abstract types `@notUndefined`; `Blob.t` and `File.t` are not marked.

- Implementation: `typeopt.ml:type_cannot_contain_undefined`.
- Tests: `spec_test` rule 12; `UntaggedVariants.res`
  (`OptionUnboxingHeuristic`); `tests/tests/src/option_wrapping_test.res`;
  `tests/build_tests/option_transitive_dependency`.
- Shaped by #6222, #6233, #8719.

### 13. `@notUndefined`

`@notUndefined` on an abstract type without a manifest declares that its
values are never `undefined`. It affects only rule 12: `option` of the type,
and of an untagged variant with a payload of the type, is unboxed. It does not
make the type a recognized payload kind (rule 4). On any other type
declaration it is an error, "@notUndefined can only be used on abstract
types". Interface inclusion does not compare it.

- Implementation: `typedecl.ml:transl_declaration`, `is_not_undefined_attr`;
  `typeopt.ml:type_cannot_contain_undefined`.
- Tests: `spec_test` rule 13; fixture `invalid_attribute_not_undefined.res`;
  `tests/build_tests/not_undefined_attribute`.
- Shaped by #7458, #7464.

### 14. `@tag` and interfaces

In a tagged variant, `@tag("f")` names the field that holds the tag of a case
with a payload (`TAG` by default), and `@as` on such a case sets the tag's
value. An inline-record field whose runtime name equals the tag field is an
error.

An interface and its implementation must agree on `@unboxed`, on `@tag`, and
on the literal of every nullary case. They are not compared on the `@as` of
cases with a payload (see Known bugs).

- Implementation: `variant_layout.ml:layout_from_type_variant`,
  `ast_untagged_variants.ml:process_tag_name`, `check_tag_field_conflicts`;
  `includecore.ml:compare_variants`, `type_declarations`.
- Tests: `spec_test` rule 14; fixtures `UntaggedTagFieldNameConflict.res`,
  `variant_tag_overlaps_with_field.res`, `UntaggedInvalidVariantTagAnnotation.res`,
  `UntaggedImplIntf.res`; `tests/build_tests/super_errors_multi/fixtures/`
  `Iface_unboxed_variant_mismatch`, `Iface_tag_name_mismatch`,
  `Iface_variant_representation_mismatch`.
- Shaped by #6498, #6513, #6669, #7875.

## Known bugs

Each was observed with the compiler built from this tree; the programs are
compiled with `bsc` and run in Node 25.

**`Some` of a constant `undefined` case is `None`.** Rule 12 boxes `Some(v)`
for a variant with an `@as(undefined)` case, but a constant argument skips
the boxing.

```rescript
@unboxed type u = | @as(undefined) U | V(string)
let x: option<u> = Some(U)                       // let x = undefined;
@inline(never) let mk = (v: u): option<u> => Some(v)
Console.log2(x == None, mk(U) == None)           // true false
```

`lambda.ml:const_some_none_depth` returns "not a none" for every
`Const_constructor`, including one whose literal is `undefined`; that this is
the cause is conjectured, not established.

**Overlapping instance classes follow declaration order** (#8938).

```rescript
@unboxed type b = B(Blob.t) | F(File.t)
let f = v => switch v { | B(_) => "blob" | F(_) => "file" }
Console.log(f(F(%raw(`new File(["x"], "a.txt")`))))   // blob
```

A `File` is a `Blob`, and the `Blob` check runs first.

**Typed arrays are classified by the name as written.** The typed-array types
are one type, so a value of one can be wrapped in the case of another.

```rescript
@unboxed type ta = I8(Int8Array.t) | U8(Uint8Array.t)
let f = v => switch v { | I8(_) => "i8" | U8(_) => "u8" }
Console.log(f(I8(Uint8Array.fromArray([1]))))   // u8
```

**Payload-case tags are not compared across a signature.** Rule 14 compares
only the literals of nullary cases.

```rescript
module M: {
  type t = A({x: int}) | C({x: int})
  let a: t
} = {
  type t = | @as("C") A({x: int}) | @as("A") C({x: int})
  let a = A({x: 1})
}
Console.log(switch M.a { | M.A(_) => "A" | M.C(_) => "C" })   // C
```

## Open questions

These are the decisions a general mechanism for recognizing payloads by class
or `typeof` kind, declared on types rather than listed in the compiler (rule
5), has to make.

1. **Unchecked claims.** Is a declared class or kind checked against the type
   it is declared on, or trusted? An unknown payload must be the only payload
   case (rule 6); a recognized one may sit beside others, so a wrong claim
   moves values between cases. Related: #7147.
2. **Subclasses.** How does the compiler learn that one class extends another,
   and does a superclass case exclude a subclass case of the same variant
   regardless of declaration order (Known bugs, #8938)? What happens to a
   value upcast to the superclass and wrapped in its case (rule 7 has the same
   effect for literals)? Related: #8806, #8914.
3. **Overlaps across kinds.** Rule 6 counts cases per kind and per class.
   Which overlaps are rejected or ordered: a class and the object case
   (`Object`), a class and the function case (`Function`), two unrelated
   classes, a declared `typeof` kind and a primitive case or literal?
   Related: #6123, #6207, #7289, #8251.
4. **The unchecked branch.** Rule 10 sends values outside every case to the
   last case or to no arm. Is that the contract, or is the last case checked
   too? Related: #6103.
5. **Realms.** `instanceof` is false across realms and overridable through
   `Symbol.hasInstance`; arrays use `Array.isArray`. Is realm-local
   recognition part of the contract? Related: #6103, #2117.
6. **Interfaces and functors.** Inclusion compares `@unboxed` and tags (rule
   14) but not `@notUndefined`. Does it compare declared classes and kinds?
   Related: #6498, #6513, #6669.
7. **Implied `@notUndefined`.** Does a declared class or kind imply that the
   type never holds `undefined` (rule 12), and how is that kept consistent
   with the declared type's own structure? Related: #6222, #6233, #7458.
8. **Abbreviations.** Rule 5 reads the name as written before expanding; two
   equal types classify differently (Known bugs). Does a declaration on an
   abbreviation follow the same rule? Related: #6394, #7788.
9. **Globals.** Classes are read as globals. How is a class shadowed by a
   local binding, imported from a module, or missing at run time handled?
   Related: #8785.
