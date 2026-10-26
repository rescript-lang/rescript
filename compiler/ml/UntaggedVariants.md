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

`@unboxed` on a variant type makes the whole variant untagged: every case with
a payload is represented by its payload. `@tag(...)` on the type is copied
onto every constructor. A constructor cannot have its own `@unboxed`: it would
mix tagged and untagged cases in one variant, and is an error telling to write
it on the type. For a constructor whose payload is an inline record with one
field, `@unboxed` on the type makes the value that field (the legacy unboxed
representation), so the error also tells to give the record type a name to
keep the record object.

| Declaration | Value | JavaScript |
| --- | --- | --- |
| `@unboxed type whole = WA(int) \| WB({x: int}) \| WC` | `WA(3)` | `3` |
| | `WB({x: 1})` | `{x: 1}` |
| | `WC` | `"WC"` |

| Declaration | Error | Fixture |
| --- | --- | --- |
| `type t = \| @unboxed A(int) \| B({x: int}) \| C` | @unboxed applies to a whole variant: write it on the type, ... | `unboxed_on_constructor.res` |
| `type t = \| @unboxed A({x: int})` | the same, and how to keep the record object | `unboxed_on_single_field_record_constructor.res` |

The layout of a declaration is computed once, after its recursive group is in
the environment, and stored on the declaration.

- Implementation: `typedecl.ml` (the constructor check and
  `copy_tag_attr_from_decl` in `transl_declaration`; the layout pass in
  `transl_type_decl`), `variant_layout.ml:layout_from_type_variant`.
- Tests: `spec_test` rule 1; the fixtures above.
- Shaped by #7320 (recursive groups), #8579 (layout stored once), #9020 and
  #9038 (constructor-level `@unboxed` removed).

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

- Implementation: `ast_untagged_variants.ml:declared_tag` (a case without
  `@as` is its name, resolved once in the layout), `translcore.ml:transl_exp0`
  (constructor application).
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
| a record (not `@unboxed`), an inline record, a JavaScript object type `{"x": int}`, `dict<_>` | object | `typeof x === "object"` |
| `array<_>`, a tuple | instance `Array` | `Array.isArray(x)` |
| a type declared with `@instanceOf("C")` (rule 5) | instance `C` | `x instanceof C` |
| `promise<_>` | instance `Promise` | `x instanceof Promise` |
| a type declared with `@typeOf("k")` | kind `k` | `typeof x === "k"` |
| anything else: an abstract type, a type variable, a variant, `list<_>`, an `@unboxed` record | unknown | no check |

The builtin types state their kinds in their declarations, as the
annotations of rule 5 do for declared types. The shape is read from the type
as written and from each type it abbreviates, and a tuple, a function type
or a record is classified after expanding abbreviations, private ones
included: `type ints = array<int>` is an array case, and `type name =
private string` a string case. Every
reader of rule 5 uses this one classification. A type is never recognized by
its name. `@typeOf("number")`
is the number kind, and `@typeOf("symbol")` is a kind of its own, with no
builtin payload type. `@notUndefined` on an abstract type leaves it unknown.

- Implementation: `predef.ml` (the builtin shapes),
  `variant_layout.ml:payload_kind`, `get_block_type`, `recognized_as`.
- Tests: `spec_test` rules 4 (`kinds`, `tuples`, `aliasDict`, `instances`,
  `unknown`); fixtures `UntaggedConstructorMoreThanOneArg.res`,
  `UntaggedConstructorAttributeMoreThanOneArg.res`, `UntaggedNonUnary1.res`,
  `UntaggedNonUnary2.res` ("Constructor A has more than one argument."),
  `UntaggedUnknown.res` (`list<float>` is unknown),
  `untagged_unannotated_stdlib_name.res` (a type named like a stdlib class
  is unknown); `tests/tests/src/untagged_runtime_annotations_test.res`,
  `untagged_builtin_alias_test.res`.
- Shaped by #6121, #6208, #6219, #6279, #6368, #6394, #6550, #6670.

### 5. Declared runtime shapes

Three annotations declare what the values of a type are at run time, each
level implying the one before:

- `@notUndefined`: they are never `undefined`;
- `@typeOf("k")`: their `typeof` is `k`, one of `"string"`, `"number"`,
  `"bigint"`, `"boolean"`, `"symbol"`, `"function"` and `"object"`;
- `@instanceOf("C")`: they are instances of the global class at the path `C`,
  such as `"Date"` or `"Intl.Locale"`.

A type has at most one of the three, and only abstract types and type
abbreviations, public or private, can have one.
`@notUndefined` affects only rule 12: `option` of the type, and of an untagged
variant with a payload of the type, is unboxed. It does not make the type a
recognized payload kind (rule 4).

When a type is declared, its annotation is resolved into its *shape*, which
is stored on the declaration: "never undefined", the `typeof` kind, or the
class with its superclasses. The builtin types state their shapes in their
declarations (rule 4). The classes annotated further down a chain of
abbreviations are the superclasses (rule 8):

```rescript
@instanceOf("Animal") type animal
@instanceOf("Dog") type dog = private animal
```

The classifier and every other reader take the shape of the payload type as
written, or of the first type it abbreviates that states one, so the
outermost annotation decides. A shape is resolved in the environment the
type is declared in, so an interface's shape is what its users see, even
where the implementation knows more. An annotation that repeats
the abbreviated type's, as the declarations of a module alias do, adds no
superclass.

An annotation on an abbreviation must agree with the abbreviated type. It can
refine a type whose representation the compiler does not see, an abstract
type that is not builtin and has no annotation, or restate what the
abbreviated type is known to be (rule 4), as `@typeOf("number") type count =
int`, `@typeOf("function") type callback = int => int` and `@notUndefined
type count = int` do; a restating `@notUndefined` keeps the
kind of `int`. The type of a functor parameter, of a
recursive module being defined, and a locally abstract type `(type a)` are
abstract only where they are declared: they stand for another type, so they
cannot be refined. Only a private abbreviation can declare a subclass of the
class annotated there, or of a `@typeOf("object")` type, also in a phantom
type hierarchy (`type dogLike<'a> = private animalLike<_dog<'a>>`). A public
`type dog = animal` would make every animal a dog without a coercion, and so
would `type dog = animal<int>` or `type dog = phantom<int>` with
`type phantom<'a> = animal`.

A class introduced on an abbreviation of a type the compiler does not see is
private too: `@instanceOf("Element") type element = private node`. A public
abbreviation would make every `node` an `element`, and a second one could
declare the same values instances of another class while the type checker
treats both as one type. The stdlib's typed arrays are such private
abbreviations of `TypedArray.t<_>`, so `Int8Array.t` and `Uint8Array.t` are
different types, upcast to `TypedArray.t<int>` with `:>`. Each typed array
module has the functions of `TypedArray` for its own type, through the
`TypedArray.Impl` functor.

| Declaration | Error | Fixture |
| --- | --- | --- |
| `@typeOf("string") type t = option<string>`, `type t<'a> = 'a`, an abbreviation of `unit`, `list<_>` or a variant | @typeOf on a type abbreviation must describe an abstract type, ... | `runtime_annotation_on_option.res`, `runtime_annotation_on_type_variable.res` |
| `@notUndefined type t = option<int>` | @notUndefined on a type abbreviation must describe an abstract type, ... | `not_undefined_on_option.res`, `tests/build_tests/not_undefined_attribute` |
| `@typeOf("number") type t = sym` with `@typeOf("symbol") type sym`, `@typeOf("string") type t = int`, or an abbreviation of a function type, a record or a tuple | This annotation, @typeOf("number"), contradicts the abbreviated type, whose values are recognized by @typeOf("symbol"). An array or tuple is recognized by Array.isArray. | `runtime_annotation_contradicts_manifest.res`, `runtime_annotation_contradicts_builtin.res`, `runtime_annotation_contradicts_function.res`, `runtime_annotation_contradicts_tuple.res` |
| `@instanceOf("Date") type date = X.t` in a functor of `X`, or over `(type a)`; `@notUndefined type t = X.t` | @instanceOf cannot describe the type of a functor parameter, ... | `instance_of_functor_parameter.res`, `type_of_locally_abstract_type.res`, `not_undefined_on_functor_parameter_type.res` |
| `@instanceOf("Dog") type dog = animal`, `= animal<int>`, or `= phantom<int>` with `type phantom<'a> = animal` | A subclass must be a private abbreviation ... | `instance_of_public_subclass.res`, `instance_of_public_subclass_instance.res`, `instance_of_public_subclass_through_alias.res` |
| `@instanceOf("Element") type element = node` with `type node` | A class must be declared on an abstract type or a private abbreviation ... | `instance_of_public_abbreviation.res`; `typed_array_cases_distinct.res` (typed arrays are distinct types) |
| `@instanceOf("Animal") type t = private dog` | Animal appears twice in this chain of abbreviations ... | `instance_of_class_twice_in_chain.res` |
| `@instanceOf("MyList") type t = private array<int>` | @instanceOf("MyList") cannot describe the values of an array type: ... subclasses of Array are not supported. | `instance_of_array_subclass.res` |
| any of the three on a record or variant | @instanceOf can only be used on abstract types and type abbreviations ... | `runtime_annotation_on_record.res`, `invalid_attribute_not_undefined.res` |
| two of the three | A type can have only one of @notUndefined, @instanceOf and @typeOf. ... | `runtime_annotation_conflict.res`, `runtime_annotation_not_undefined_conflict.res` |
| one annotation twice | Duplicate @instanceOf annotation; a type can have only one. | `instance_of_duplicate.res` |

What an interface states about a type must hold for its implementation. A
class or `typeof`, stated through its own annotation or the type it
abbreviates, must be the one the implementation has by rule 4, superclasses
included: "Their runtime representations differ"
(`runtime_annotation_interface_over_record.res`). A builtin type states its
kind, so `@typeOf("number") type t` in an interface accepts `type t = int`
(`untagged_builtin_alias_test.res`), and `@typeOf("function") type t` accepts
`type t = int => int` (`untagged_runtime_annotations_test.res`). Under `@notUndefined`, the
implementation must be a type that cannot hold `undefined` either, or one
whose representation the compiler does not see, which has none to contradict
it: an abstract type, also through an abbreviation such as
`type t = Dom.element`, or a JavaScript object type; a functor parameter's or
a locally abstract type stands for another type and does not count.
`type t = option<int>` under `@notUndefined type t` is an error: "the second
declaration is @notUndefined, but the first declaration can hold undefined".
An interface that states nothing hides the shape. The same holds for functor
arguments, `with type` constraints and module type ascriptions. Within one
variant a class is identified by its path, so two types of the same class are
one case (rule 6); across a signature the two declarations must also state
the same superclasses.

The class path is a dot-separated list of identifiers whose first one is not
a JavaScript keyword. Paths that name no class to recognize values by are
rejected as well:

| Path | Error | Fixture |
| --- | --- | --- |
| not a path, or a keyword global | @instanceOf expects the name of a global class ... | `instance_of_invalid_class.res`, `instance_of_keyword.res` |
| `globalThis.C` | Write @instanceOf("C") without "globalThis." ... | `instance_of_global_this.res` |
| `Object`, `Function` | Every object is an instance of Object ... Use @typeOf("object") instead. | `instance_of_object.res` |
| `Array` | Arrays are recognized by Array.isArray ... | `instance_of_array.res` |
| `String`, `Number`, `Boolean`, `BigInt`, `Symbol` | Values of type bigint are primitives ... | `instance_of_primitive_wrapper.res` |
| `globalThis`, `undefined`, `NaN`, `Infinity`, `Math`, `JSON`, `Reflect`, `Atomics`, `Intl`, `Proxy` | Math is not a class ... | `instance_of_not_a_class.res` |

An unknown `typeof` is an error too (`type_of_invalid.res`). The class is read
as a global; where a local binding of the same name is in scope, it is read
through `globalThis` (#8785). The compiler trusts the annotations, as it
trusts the type of an external: a value that is not what its type declares is
recognized by what it is (rule 10).

The stdlib annotates the types of the classes it binds:

| Type | Annotation |
| --- | --- |
| `Date.t`, `RegExp.t`, `ArrayBuffer.t`, `DataView.t`, `Map.t`, `Set.t`, `WeakMap.t`, `WeakSet.t`, `Blob.t` | `@instanceOf` the class of the same name |
| `File.t`, a private abbreviation of `Blob.t` | `@instanceOf("File")`, a subclass of `Blob` |
| `JsError.t` | `@instanceOf("Error")` |
| `Intl.Collator.t`, `Intl.DateTimeFormat.t`, `Intl.ListFormat.t`, `Intl.Locale.t`, `Intl.NumberFormat.t`, `Intl.PluralRules.t`, `Intl.RelativeTimeFormat.t`, `Intl.Segmenter.t` | `@instanceOf("Intl.Collator")` and so on |
| `Int8Array.t` and the other typed arrays, public abbreviations of `TypedArray.t<_>` | `@instanceOf` the class of the same name |
| `Symbol.t` | `@typeOf("symbol")` |

The typed-array types are abbreviations of one type, `TypedArray.t<_>`, and
are told apart only by their annotations, which are read on the name as
written (Known bugs).

- Implementation: `types.mli:type_shape`,
  `variant_runtime.mli:value_shape`; `variant_layout.ml:declaration_shape`,
  `resolve_shapes`, `value_shape`, `payload_kind`;
  `typedecl.ml:check_manifest_annotation`; `typeopt.ml:abstractness`,
  `undefinedness`; `ast_untagged_variants.ml:process_runtime_annotation`,
  `unsupported_class`, `check_runtime_annotation`;
  `includecore.ml:compare_runtime_annotations`;
  `variant_runtime.ml:Instance`; `js_exp_make.ml:class_constructor`.
- Tests: `spec_test` rules 4 and 5;
  `tests/tests/src/untagged_runtime_annotations_test.res`,
  `untagged_runtime_annotations_shadow_test.res`,
  `untagged_file_blob_test.res`, `not_undefined_interface_test.res`,
  `not_undefined_abbreviation_test.res`; the fixtures above,
  `UntaggedAtMostOneInstance.res`, `UntaggedTupleAndArray.res`,
  `not_undefined_functor_argument.res`, `not_undefined_functor_parameter.res`;
  `tests/build_tests/super_errors_multi/fixtures/`
  `Cross_runtime_annotation_hidden_superclass`,
  `Cross_runtime_annotation_hidden_base`,
  `Cross_runtime_annotation_claimed_by_interface`,
  `Cross_runtime_annotation_functor_argument`,
  `Cross_not_undefined_option_implementation`.
- Shaped by #6296, #6383, #6396, #7458, #7464, #7788, #8009, #8950, #8948,
  #8949, #8955, #8964. Related: #7147.

### 6. Well-formedness

The type checker rejects an untagged variant, with
"This untagged variant definition is invalid: ..." and the reason below, when:

| Condition | Reason | Fixture |
| --- | --- | --- |
| two object cases | At most one case can be an object type. | `UntaggedAtMostOneObject.res` |
| two cases of one instance class, compared by path (arrays and tuples share `Array`) | At most one case can be an instance of C; for `Array`, an array or tuple type. | `UntaggedAtMostOneInstance.res`, `UntaggedTupleAndArray.res`, `untagged_same_class_twice.res` |
| two function cases | At most one case can be a function type. | `UntaggedAtMostOneFunction.res` |
| two string cases | At most one case can be a string type. | `UntaggedAtMostOneString.res` |
| two number cases (`int` and `float` count together) | At most one case can be a number type (int or float). | `UntaggedAtMostOneNumber.res` |
| two bigint cases | At most one case can be a bigint type. | `UntaggedAtMostOneBigint.res` |
| two boolean cases | At most one case can be a boolean type. | `UntaggedAtMostOneBoolean.res` |
| two symbol cases | At most one case can be a symbol type. | `untagged_two_symbols.res` |
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

- Implementation: `variant_layout.ml:check_invariant`,
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

The literals are compared with `===`: by one `switch (x)` over the literal
arms, whose default holds the payload dispatch, or, when at most two
literals get a `case`, by an `if` per arm body. A literal without an arm of its own goes
to the default arm. It gets a `case` leading there when it could pass a
check of the payload dispatch (a string literal beside a `string` arm, any
literal beside an unknown-kind arm) or when the variant has class cases;
otherwise it reaches the default through the payload dispatch, which then
checks its last arm. A `null` literal that only the object check would
accept adds `x !== null` to that check instead (rule 9).

A payload pattern whose every value is a literal case's can therefore never
match, and is reported: warning 11 for an arm, such as `S("a")` beside
`@as("a") A`, and warning 12 for an alternative of an or-pattern, both with
"\"a\" is the runtime value of constructor A, so it is always matched as A,
never as S". Values are compared as `===` compares them (rule 6), so
`BI(02n)` beside `@as(2n) Two` and `P(#Foo)` beside `@as("Foo") F` are
reported too, and an or-pattern also counts when it is named with `as`. An
untagged payload inside another is the value itself, so the outer literal
cases also shadow it: `W(N(1 | 2))` reports `1` beside `@as(1) One | W(inner)`. For the
exhaustiveness check a literal case covers the payload values equal to it, so
`Yes | Ans(false)` is a complete match of `@as(true) Yes | Ans(bool)`. This
needs a payload of type `bool`, also through an abbreviation: a
`@typeOf("boolean")` type has no `true` and `false` patterns, so `Yes` alone
does not cover it.

- Implementation: `lam_compile.ml:compile_switch`;
  `parmatch.ml` (shadowed payload patterns, exhaustiveness).
- Tests: `spec_test` rule 7; `tests/tests/src/untagged_variants_heuristic.res`
  (literals that skip the `switch`); `unboxed_variant_fold_test.res`
  (`answerName2`, `bigintOrPayloadName`); fixtures
  `warning_11_untagged_literal_shadows_payload.res`,
  `warning_11_untagged_literal_shadows_payload_kinds.res`,
  `warning_11_untagged_literal_shadows_bigint.res`,
  `warning_11_untagged_bool_payload_overlap.res`,
  `warning_8_untagged_boolean_kind_payload.res`,
  `warning_11_untagged_literal_shadows_polyvariant.res`,
  `warning_12_untagged_literal_shadows_or_pattern.res`,
  `warning_12_untagged_literal_shadows_aliased_or_pattern.res`,
  `warning_12_untagged_literal_shadows_nested_payload.res`.
- Shaped by #6125, #7128, #7135, #8806.

### 8. Payload cases

The payload arms compile to one `switch (typeof x)` over the arms of
primitive kinds (string, number, bigint, boolean, symbol), an `if` for one
kind, whose default is an `if` chain over the other arms:

1. the instance cases (arrays, tuples and classes), a subclass before its
   superclasses (rule 5), otherwise in declaration order;
2. the function case, `typeof x === "function"`;
3. the object case, `typeof x === "object"`;
4. the unknown-kind case, unchecked.

```js
switch (typeof x) {
  case "string": ...
  case "number": ...
  default:
    if (x instanceof Dog) { ... }
    else if (x instanceof Animal) { ... }
    else if (typeof x === "object") { ... }
}
```

An instance case without an arm leads to the default arm, so an array or a
class instance never lands in the function or object case. Before the last
checked arm such checks are folded into its check, which then guards them:
`typeof x === "object" && !(x instanceof Date)`. A primitive never reaches a
class check: it is taken by the `switch (typeof x)`, where a primitive kind
of the variant without an arm has a `case` leading to the default when a
class check could follow. In a complete match the last arm is not checked
(rule 10).

A value upcast to the superclass and wrapped in its case lands in the
subclass case, as a payload equal to a literal lands in the literal's case
(rule 7). `instanceof` consults `Symbol.hasInstance`, which is trusted for
the values that reach the check: objects and functions, and values outside
every case (rule 10).
An `int` case is recognized by `typeof x === "number"` and so receives every
number, integral or not.

`break` and `continue` in an arm leave or continue the enclosing loop: every
arm placed in a generated `switch` is compiled in a `switch` context, where
they are labeled.

- Implementation: `lam_compile.ml:compile_untagged_cases`,
  `compile_general_cases`.
- Tests: `spec_test` rules 4, 5 and 8 (`instanceName` shows the chain in
  declaration order, `objectFirst` the unchecked last arm);
  `tests/tests/src/UntaggedVariants.res` (`AllInstanceofTypes`,
  `MergeCases`); `tests/tests/src/untagged_runtime_annotations_test.res`
  (subclasses, the function case, a missing class);
  `untagged_default_arm_test.res` (`Symbol.hasInstance`, incomplete
  matches); `untagged_loop_control_test.res` (`break`, `continue`).
- Shaped by #6103, #6383, #6792, #7144, #7149.

### 9. `null` and arrays are not objects

`typeof null` and `typeof []` are `"object"`, and a class instance is an
object or a function. The object case never receives `null` when the variant
has an `@as(null)` case: the literal comparisons run first (rule 7), or the
object check tests `x !== null` when `null` goes to the default arm. Neither
the object nor the function case receives an instance of another case: the
instance checks come first (rule 8). A case declared with `@typeOf("object")`
(rule 5) is an object case: without an `@as(null)` case, it receives `null`,
as a record case does.

| Declaration | `switch` arms | Value | Arm |
| --- | --- | --- | --- |
| `GArray(array<int>) \| GObject({x: int}) \| @as(null) GNull` | all | `[]` | `GArray` |
| | all | `null` | `GNull` |
| | `GObject(_)`, `_` | `[]`, `null` | `_` |

- Implementation: `lam_compile.ml:compile_switch` (`null_guard`),
  `compile_untagged_cases`.
- Tests: `spec_test` rule 9; `UntaggedVariants.res` (`ObjectAndNull`, `Json`);
  `tests/tests/src/untagged_default_arm_test.res`.
- Shaped by #6219, #7303, #8253, #8255, #8279, #8995.

### 10. Values outside every case

Recognition trusts the type. A value that belongs to no case is not
rejected: in a complete match it lands in the arm that is not checked, the
last one of the `if` chain, or the last primitive arm when there is no other
arm. When the match sends several cases to one shared default, the dispatch
checks the arms before it and the value lands in that default instead. With `I(int) | S(string)`, `true` lands in `S`; with
`TDate(Date.t) | TRegExp(RegExp.t)`, `{x: 1}` lands in `TRegExp`; with
`PPromise(promise<int>) | PFloat(float)`, the float case is checked, so a
thenable that is not a `Promise` lands in `PPromise`.

A value from another JavaScript realm fails every `instanceof` check and is
recognized the same way (by the semantics of `instanceof`; not tested):
recognition is local to a realm. A class that does not exist where the
`switch` runs makes its check throw for every value that reaches it: objects
and functions, and values outside every case. The literals and the
primitives of the variant never reach it.

- Implementation: `lam_compile.ml:compile_untagged_cases` (a complete match
  emits no check for its last arm).
- Tests: `spec_test` rule 10; `untagged_runtime_annotations_test.res` (a
  missing class).

### 11. Constant folding

A `switch` on an untagged value the compiler knows, a literal or a block of
constants, folds to the arm that the run-time dispatch selects for its
JavaScript value, so `S("a")` matched against `@as("a") A | S(string)` folds
to `A`. A value built at run time, such as `A([1])` with its array, is
matched at run time.

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
| a type declared with `@notUndefined`, `@instanceOf` or `@typeOf` (rule 5), an `@unboxed` wrapper of one | no |
| an abstract type without it, a type variable, `option<_>` | yes |
| a type whose declaration is not on the load path | yes |

An `@as(null)` case does not prevent unboxing; `Some(N)` is `null`. `Some(U)` of an `@as(undefined)` case is boxed, as a constant
too, so it stays distinct from `None`.

| Declaration | `Some(v)` |
| --- | --- |
| `@as(null) WNull \| WString(string)` | `v` |
| `@as(undefined) WUndefined \| WUString(string)` | `Primitive_option.some(v)` |
| `@as("n") ONone \| OPaque(opaque)` | `Primitive_option.some(v)` |
| `BBlob(Blob.t) \| BString(string)` | `v` |
| `DDate(Date.t) \| DString(string)` | `v` |
| `RRecord(record) \| RString(string)` with `type record = {y: option<int>}` | `v` |
| `INone \| IRecord({x: option<int>})` | `Primitive_option.some(v)` |
| `@as("n") UONone \| UOption(unboxedOption)` with `@unboxed type unboxedOption = {x: option<int>}` | `Primitive_option.some(v)` |
| `@as("n") USNone \| UString(unboxedString)` with `@unboxed type unboxedString = {s: string}` | `v` |

The stdlib's annotated types (rule 5) cannot hold `undefined`. It marks the
JSX element and component types, `Iterator.t`, `AsyncIterator.t`,
`TypedArray.t`, `Intl.Segments.t`, `Intl.NumberFormat.Grouping.t` and
`Lazy.t` with `@notUndefined`.

Rule 5 keeps an annotated abbreviation from claiming this of a type that can
hold `undefined`.

- Implementation: `typeopt.ml:type_cannot_contain_undefined`.
- Tests: `spec_test` rule 12; `UntaggedVariants.res`
  (`OptionUnboxingHeuristic`); `tests/tests/src/option_wrapping_test.res`;
  `tests/build_tests/option_transitive_dependency`.
- Shaped by #6222, #6233, #8719, #8954.

### 13. `@tag` and interfaces

In a tagged variant, `@tag("f")` names the field that holds the tag of a case
with a payload (`TAG` by default), and `@as` on such a case sets the tag's
value. An inline-record field whose runtime name equals the tag field is an
error.

An interface and its implementation must agree on `@unboxed`, on `@tag`, on
the literal of every nullary case and of every tagged case with a payload,
and on the payload kind of every untagged case whose kind the interface
knows. A case without `@as` has the literal of its name, so `A` and
`@as("A") A` agree, here and in coercions (`as_constructor_name_test.res`). An unknown payload in the interface is its only payload case and is
not checked, so it agrees with any payload of the implementation
(`untagged_hidden_payload_test.res`). The `@as` of an untagged case with a
payload does not change its representation (rule 2) and is not compared.
Type declarations in messages print the `@as` of nullary and tagged cases.

- Implementation: `variant_layout.ml:layout_from_type_variant`,
  `ast_untagged_variants.ml:process_tag_name`, `check_tag_field_conflicts`;
  `includecore.ml:compare_variants`, `type_declarations`.
- Tests: `spec_test` rule 13; fixtures `UntaggedTagFieldNameConflict.res`,
  `variant_tag_overlaps_with_field.res`, `UntaggedInvalidVariantTagAnnotation.res`,
  `UntaggedImplIntf.res`; `tests/build_tests/super_errors_multi/fixtures/`
  `Iface_unboxed_variant_mismatch`, `Iface_tag_name_mismatch`,
  `Iface_variant_representation_mismatch`; `variant_payload_tag_mismatch.res`.
- Shaped by #6498, #6513, #6669, #7875, #8956.

## Known bugs

Each was observed with the compiler built from this tree; the programs are
compiled with `bsc` and run in Node 25.

**Typed arrays share one type** (#8955). The typed-array types are public
abbreviations of `TypedArray.t<_>`, each annotated with its class, so a value
of one can be wrapped in the case of another.

```rescript
@unboxed type ta = I8(Int8Array.t) | U8(Uint8Array.t)
let f = v => switch v { | I8(_) => "i8" | U8(_) => "u8" }
Console.log(f(I8(Uint8Array.fromArray([1]))))   // u8
```

## Decisions on declared classes and kinds

The questions a mechanism for declaring classes and `typeof` kinds on types
had to answer, and the answers of rule 5:

1. **Unchecked claims.** An annotation is trusted, as the type of an external
   is, but checked against the type it abbreviates (rule 5).
2. **Subclasses.** A class's superclasses are the classes annotated further
   down its chain of private abbreviations. A superclass case excludes its subclass cases in
   any order, and an upcast value lands in the subclass case (rule 8). The
   relation must be visible where the variant is declared: an interface that
   hides it is rejected (rule 5).
3. **Overlaps across kinds.** `Object`, `Function`, `Array` and the wrappers
   of primitives are rejected as classes. A declared kind counts as the
   builtin kind (rule 6). Unrelated classes are tested in declaration order.
4. **The unchecked branch** stays the contract (rule 10).
5. **Realms.** Recognition is local to a realm (rule 10).
6. **Interfaces and functors** compare declared shapes (rule 5) and the
   payload kinds of untagged cases (rule 13).
7. **Implied `@notUndefined`.** A declared class or kind implies it, and
   rule 5 rejects annotations on abbreviations of types that can hold
   `undefined`.
8. **Abbreviations.** Annotations are read along the chain of abbreviations,
   and a type is never recognized by its name.
9. **Globals.** A shadowed class is read through `globalThis`. A missing
   class throws when the check runs (rule 10).

Still open: a class imported from a module cannot be named, and the typed
arrays share one type (#8955).
