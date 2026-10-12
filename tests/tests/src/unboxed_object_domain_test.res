open Mocha
open Test_utils

@inline(never) let id = value => value

module Domain = {
  type details = {count: int}
  type generic<'kind> = {kind: 'kind, value: int}
  type textAlias = string
  @unboxed
  type t =
    | Text(generic<textAlias>)
    | Number({kind: float, number: float})
    | Flag({kind: bool, enabled: bool})
    | Big({kind: bigint, big: bigint})
    | Missing({kind: unit, message: string})
    | Items({kind: array<int>, size: int})
    | Object({kind: details, count: int})
    | Callback({kind: int => int, argument: int})
    | Date({kind: Date.t, label: string})
    | Regex({kind: RegExp.t, label: string})

  @inline(always)
  let name = value =>
    switch value {
    | Text({value}) => "text:" ++ Int.toString(value)
    | Number({number}) => "number:" ++ Float.toString(number)
    | Flag({enabled}) => enabled ? "enabled" : "disabled"
    | Big({big}) => "big:" ++ BigInt.toString(big)
    | Missing({message}) => message
    | Items({size}) => "items:" ++ Int.toString(size)
    | Object({count}) => "object:" ++ Int.toString(count)
    | Callback({kind, argument}) => "callback:" ++ Int.toString(kind(argument))
    | Date({label}) | Regex({label}) => label
    }

  let externalValues: unit => array<t> = %raw(`() => [
    {kind: "x", value: 1},
    {kind: 2.5, number: 2.5},
    {kind: false, enabled: true},
    {kind: 3n, big: 3n},
    {kind: undefined, message: "missing"},
    {kind: [1, 2], size: 2},
    {kind: {count: 4}, count: 4},
    {kind: x => x + 1, argument: 4},
    {kind: new Date(0), label: "date"},
    {kind: /x/, label: "regex"}
  ]`)
}

module Literal = {
  @unboxed type stringKind = | @as("start") Start | @as("stop") Stop
  @unboxed type numberKind = | @as(1) One | @as(2) Two
  @unboxed type bigintKind = | @as(1n) BigOne | @as(2n) BigTwo
  @unboxed type boolKind = | @as(true) Yes | @as(false) No
  @unboxed type nullKind = | @as(null) NullKind
  @unboxed type missingKind = | @as(undefined) MissingKind
  @unboxed
  type t =
    | Text({kind: stringKind, text: string})
    | Number({kind: numberKind, number: int})
    | Big({kind: bigintKind, big: bigint})
    | Flag({kind: boolKind, enabled: bool})
    | Null({kind: nullKind, message: string})
    | Missing({kind: missingKind, message: string})

  let name = value =>
    switch value {
    | Text({text}) => text
    | Number({number}) => Int.toString(number)
    | Big({big}) => BigInt.toString(big)
    | Flag({enabled}) => enabled ? "yes" : "no"
    | Null({message}) | Missing({message}) => message
    }

  let externalValues: unit => array<t> = %raw(`() => [
    {kind: "stop", text: "text"}, {kind: 2, number: 2},
    {kind: 2n, big: 2n}, {kind: false, enabled: false},
    {kind: null, message: "null"}, {kind: undefined, message: "undefined"}
  ]`)
}

module Combined = {
  @unboxed type textOrNumber = Text(string) | Number(int)
  @unboxed type boolOrBigint = Flag(bool) | Big(bigint)
  type textRecord = {@as("runtime-kind") tag: textOrNumber, value: int}
  @unboxed
  type t =
    | Scalar(textRecord)
    | Other({@as("runtime-kind") kind: boolOrBigint, reason: string})
    | Pair({@as("runtime-kind") kind: (int, int), total: int})
    | @as(null) Null
    | Text(string)
    | Array(array<int>)

  let name = value =>
    switch value {
    | Scalar({value}) => "scalar:" ++ Int.toString(value)
    | Other({reason}) => reason
    | Pair({total}) => "pair:" ++ Int.toString(total)
    | Null => "null"
    | Text(text) => text
    | Array(values) => "array:" ++ Int.toString(Array.length(values))
    }
  let partial = value =>
    switch value {
    | Scalar({value}) => value
    | _ => -1
    }
  let externalValues: unit => array<t> = %raw(`() => [
    {"runtime-kind": "a", value: 1}, {"runtime-kind": 42, value: 2},
    {"runtime-kind": true, reason: "bool"}, {"runtime-kind": 2n, reason: "big"},
    {"runtime-kind": [1, 2], total: 3}, null, "outer", [1, 2]
  ]`)
}

module BoxedField = {
  type kind = Empty | Detail(int)
  @unboxed
  type t = Boxed({kind: kind, value: int}) | Flag({kind: bool, reason: string})
  let name = value =>
    switch value {
    | Boxed({kind: Empty, value}) => "empty:" ++ Int.toString(value)
    | Boxed({kind: Detail(number), value}) => "detail:" ++ Int.toString(number + value)
    | Flag({reason}) => reason
    }
  let externalValues: unit => array<t> = %raw(`() => [
    {kind: "Empty", value: 1}, {kind: {TAG: "Detail", _0: 2}, value: 3},
    {kind: true, reason: "flag"}
  ]`)
}

module ListField = {
  @unboxed
  type t = Values({kind: list<int>, value: int}) | Flag({kind: bool, reason: string})
  let name = value =>
    switch value {
    | Values({kind: list{}, value}) => "empty:" ++ Int.toString(value)
    | Values({value}) => "list:" ++ Int.toString(value)
    | Flag({reason}) => reason
    }
  let externalValue: unit => t = %raw(`() => ({kind: 0, value: 1})`)
}

module ShadowedBuiltin = {
  // This local name must not be mistaken for the built-in object dictionary.
  type dict<'a> = Yes | No
  @unboxed
  type t = Enum({kind: dict<int>, value: int}) | Number({kind: int, reason: string})
  let name = value =>
    switch value {
    | Enum({kind: Yes, value}) => "yes:" ++ Int.toString(value)
    | Enum({kind: No, value}) => "no:" ++ Int.toString(value)
    | Number({reason}) => reason
    }
  let externalValues: unit => array<t> = %raw(`() => [
    {kind: "Yes", value: 1}, {kind: "No", value: 2}, {kind: 42, reason: "number"}
  ]`)
}

module ShadowedDate = {
  module Stdlib_Date = {
    type t = string
  }
  @unboxed
  type t = Text({kind: Stdlib_Date.t, value: int}) | Number({kind: int, reason: string})
  let name = value =>
    switch value {
    | Text({value}) => "text:" ++ Int.toString(value)
    | Number({reason}) => reason
    }
  let externalValue: unit => t = %raw(`() => ({kind: "text", value: 1})`)
}

module TypedArrayField = {
  type details = {count: int}
  @unboxed
  type t =
    | Typed({kind: Uint8Array.t, value: int})
    | View({kind: DataView.t, reason: string})
    | Object({kind: details, count: int})
  let name = value =>
    switch value {
    | Typed({value}) => "typed:" ++ Int.toString(value)
    | View({reason}) => reason
    | Object({count}) => "object:" ++ Int.toString(count)
    }
  let externalValues: unit => array<t> = %raw(`() => [
    {kind: new Uint8Array([1]), value: 1},
    {kind: new Int16Array([2]), value: 2},
    {kind: new DataView(new ArrayBuffer(4)), reason: "view"},
    {kind: {count: 3}, count: 3}
  ]`)
}

module MutableOverlap = {
  type special = | @as("special") SpecialKind
  type broad = {kind: string, mutable value: int}
  @unboxed
  type t = Broad(broad) | Special({kind: special, mutable value: int})
  let value = value =>
    switch value {
    | Broad({value}) => -value
    | Special({value}) => value
    }
}

module OptionalDomain = {
  @unboxed
  type t =
    | @as(undefined) Missing
    | Text({kind: string, value: int})
    | Number({kind: int, reason: string})
  let partial = value =>
    switch value {
    | Text({value}) => value
    | _ => -1
    }
  let effect = (value, result) => {
    switch value {
    | Missing => result.contents = "missing"
    | Text({value}) => result.contents = "text:" ++ Int.toString(value)
    | Number({reason}) => result.contents = reason
    }
    result.contents ++ ":done"
  }
}

module Overlap = {
  @unboxed type special = | @as("special") SpecialKind
  type generic<'kind> = {kind: 'kind, @as("payload") data: int}
  type broad = generic<string>
  @unboxed
  type t =
    | Broad(broad)
    | Special({kind: special, @as("payload") value: int})
  @inline(always)
  let name = value =>
    switch value {
    | Broad({data}) => "broad:" ++ Int.toString(data)
    | Special({value}) => "special:" ++ Int.toString(value)
    }
  @inline(always)
  let partial = value =>
    switch value {
    | Broad(_) => "broad"
    | _ => "special"
    }
  let guarded = value =>
    switch value {
    | Broad(_) => "broad"
    | Special({value}) if value > 10 => "large"
    | Special(_) => "small"
    }
  let externalValue: unit => t = %raw(`() => ({kind: "special", payload: 12})`)
}

module ErasedInlineField = {
  @unboxed type wrapped<'a> = Wrapped({value: 'a})
  @unboxed type nested<'a> = Nested({inner: wrapped<'a>})
  // Two fields preserve an object, while a single immutable field is erased.
  @unboxed type objectWrapper = ObjectKind({value: string, count: int})
  @unboxed
  type t =
    | Text({kind: wrapped<string>, text: string})
    | Number({kind: wrapped<int>, number: int})
    | Flag({kind: nested<bool>, reason: string})
    | Object({kind: objectWrapper, count: int})
  @inline(always)
  let name = value =>
    switch value {
    | Text({text}) => text
    | Number({number}) => Int.toString(number)
    | Flag({reason}) => reason
    | Object({count}) => "object:" ++ Int.toString(count)
    }
  let externalValues: unit => array<t> = %raw(`() => [
    {kind: "erased", text: "text"}, {kind: 42, number: 42},
    {kind: true, reason: "nested"}, {kind: {value: "object", count: 3}, count: 3}
  ]`)

  type wrappedAlias<'a> = wrapped<'a>
  @unboxed
  type repeated =
    | Repeated({kind: wrapped<wrapped<string>>, value: int})
    | Aliased({kind: wrappedAlias<wrappedAlias<int>>, reason: string})
  @inline(always)
  let repeatedName = value =>
    switch value {
    | Repeated({value}) => "repeated:" ++ Int.toString(value)
    | Aliased({reason}) => reason
    }
  let externalRepeated: unit => array<repeated> = %raw(`() => [
    {kind: "string", value: 1}, {kind: 42, reason: "aliased"}
  ]`)

  type leftKind = | @as("left") LeftKind
  type rightKind = | @as("right") RightKind
  @unboxed
  type literal =
    | Left({kind: wrapped<leftKind>, value: int})
    | Right({kind: nested<rightKind>, reason: string})
  @inline(always)
  let literalName = value =>
    switch value {
    | Left({value}) => "left:" ++ Int.toString(value)
    | Right({reason}) => reason
    }
  let externalLiterals: unit => array<literal> = %raw(`() => [
    {kind: "left", value: 1}, {kind: "right", reason: "right"}
  ]`)
}

module OrdinaryOptionNames = {
  // Two Some arguments keep an ordinary boxed representation.
  type kind = None | Some(int, int)
  @unboxed
  type t = Value({kind: kind, value: int}) | Flag({kind: bool, reason: string})
  let name = value =>
    switch value {
    | Value({kind: None, value}) => "none:" ++ Int.toString(value)
    | Value({kind: Some(left, right), value}) => "some:" ++ Int.toString(left + right + value)
    | Flag({reason}) => reason
    }
  let externalValues: unit => array<t> = %raw(`() => [
    {kind: "None", value: 1}, {kind: {TAG: "Some", _0: 2, _1: 3}, value: 4},
    {kind: true, reason: "flag"}
  ]`)
}

module ScalarOverlap = {
  @unboxed type one = | @as(1) OneKind
  @unboxed type yes = | @as(true) YesKind
  @unboxed type bigOne = | @as(1n) BigOneKind
  @unboxed
  type t =
    | Number({kind: int, value: int})
    | One({kind: one, value: int})
    | Flag({kind: bool, value: int})
    | Yes({kind: yes, value: int})
    | Big({kind: bigint, value: int})
    | BigOne({kind: bigOne, value: int})
  @inline(always)
  let name = value =>
    switch value {
    | Number(_) => "number"
    | One(_) => "one"
    | Flag(_) => "flag"
    | Yes(_) => "yes"
    | Big(_) => "big"
    | BigOne(_) => "bigOne"
    }
}

describe(__MODULE__, () => {
  test("external objects discriminate every broad field runtime domain", () => {
    eq(
      __LOC__,
      Domain.externalValues()->Array.map(Domain.name),
      [
        "text:1",
        "number:2.5",
        "enabled",
        "big:3",
        "missing",
        "items:2",
        "object:4",
        "callback:5",
        "date",
        "regex",
      ],
    )
    eq(__LOC__, Domain.name(Domain.Text({kind: "x", value: 2})), "text:2")
    eq(__LOC__, Domain.name(id(Domain.Text({kind: "x", value: 2}))), "text:2")
    eq(__LOC__, Domain.name(Domain.Flag({kind: false, enabled: true})), "enabled")
    eq(__LOC__, Domain.name(id(Domain.Flag({kind: false, enabled: true}))), "enabled")
    eq(__LOC__, Domain.name(Domain.Missing({kind: (), message: "unit"})), "unit")
    eq(__LOC__, Domain.name(id(Domain.Missing({kind: (), message: "unit"}))), "unit")
    eq(__LOC__, Domain.name(Domain.Items({kind: [1], size: 1})), "items:1")
    eq(__LOC__, Domain.name(id(Domain.Items({kind: [1], size: 1}))), "items:1")
    eq(__LOC__, Domain.name(Domain.Object({kind: {count: 2}, count: 2})), "object:2")
    eq(__LOC__, Domain.name(id(Domain.Object({kind: {count: 2}, count: 2}))), "object:2")
  })

  test("public enum field domains include all JavaScript literal kinds", () => {
    eq(
      __LOC__,
      Literal.externalValues()->Array.map(Literal.name),
      ["text", "2", "2", "no", "null", "undefined"],
    )
    eq(__LOC__, Literal.name(id(Literal.Text({kind: Stop, text: "stop"}))), "stop")
    eq(__LOC__, Literal.name(id(Literal.Number({kind: Two, number: 2}))), "2")
    eq(__LOC__, Literal.name(id(Literal.Big({kind: BigTwo, big: 2n}))), "2")
    eq(__LOC__, Literal.name(id(Literal.Flag({kind: No, enabled: false}))), "no")
    eq(__LOC__, Literal.name(id(Literal.Null({kind: NullKind, message: "null"}))), "null")
    eq(
      __LOC__,
      Literal.name(id(Literal.Missing({kind: MissingKind, message: "missing"}))),
      "missing",
    )
  })

  test("combined field domains coexist with outer scalar, null, and array cases", () => {
    eq(
      __LOC__,
      Combined.externalValues()->Array.map(Combined.name),
      ["scalar:1", "scalar:2", "bool", "big", "pair:3", "null", "outer", "array:2"],
    )
    eq(
      __LOC__,
      Combined.externalValues()->Array.map(Combined.partial),
      [1, 2, -1, -1, -1, -1, -1, -1],
    )
    eq(__LOC__, Combined.name(id(Combined.Scalar({tag: Number(1), value: 2}))), "scalar:2")
    eq(__LOC__, Combined.name(id(Combined.Other({kind: Big(1n), reason: "big"}))), "big")
    eq(__LOC__, Combined.name(id(Combined.Pair({kind: (1, 2), total: 3}))), "pair:3")
  })

  test("boxed field variants combine literal and object runtime domains", () => {
    eq(
      __LOC__,
      BoxedField.externalValues()->Array.map(BoxedField.name),
      ["empty:1", "detail:5", "flag"],
    )
    eq(__LOC__, BoxedField.name(id(BoxedField.Boxed({kind: Empty, value: 1}))), "empty:1")
    eq(__LOC__, BoxedField.name(id(BoxedField.Boxed({kind: Detail(2), value: 3}))), "detail:5")
    eq(__LOC__, BoxedField.name(id(BoxedField.Flag({kind: false, reason: "flag"}))), "flag")
  })

  test("list fields include both empty-number and nonempty-object representations", () => {
    eq(__LOC__, ListField.name(ListField.externalValue()), "empty:1")
    eq(__LOC__, ListField.name(id(ListField.Values({kind: list{}, value: 1}))), "empty:1")
    eq(__LOC__, ListField.name(id(ListField.Values({kind: list{1, 2}, value: 2}))), "list:2")
    eq(__LOC__, ListField.name(id(ListField.Flag({kind: false, reason: "flag"}))), "flag")
  })

  test("local dict type names use their declared field representation", () => {
    eq(
      __LOC__,
      ShadowedBuiltin.externalValues()->Array.map(ShadowedBuiltin.name),
      ["yes:1", "no:2", "number"],
    )
    eq(__LOC__, ShadowedBuiltin.name(id(ShadowedBuiltin.Enum({kind: Yes, value: 1}))), "yes:1")
    eq(__LOC__, ShadowedBuiltin.name(id(ShadowedBuiltin.Enum({kind: No, value: 2}))), "no:2")
    eq(
      __LOC__,
      ShadowedBuiltin.name(id(ShadowedBuiltin.Number({kind: 42, reason: "number"}))),
      "number",
    )
  })

  test("local modules named after builtins use their declared representation", () => {
    eq(__LOC__, ShadowedDate.name(ShadowedDate.externalValue()), "text:1")
    eq(__LOC__, ShadowedDate.name(id(ShadowedDate.Text({kind: "text", value: 2}))), "text:2")
    eq(__LOC__, ShadowedDate.name(id(ShadowedDate.Number({kind: 42, reason: "number"}))), "number")
  })

  test("typed arrays share their runtime group and exclude DataView and records", () => {
    eq(
      __LOC__,
      TypedArrayField.externalValues()->Array.map(TypedArrayField.name),
      ["typed:1", "typed:2", "view", "object:3"],
    )
    eq(
      __LOC__,
      TypedArrayField.name(
        id(TypedArrayField.Typed({kind: Int16Array.fromArray([1, 2]), value: 2})),
      ),
      "typed:2",
    )
  })

  test("compatible mutable payloads retain aliases under literal precedence", () => {
    let payload: MutableOverlap.broad = {kind: "special", value: 1}
    let value = MutableOverlap.Broad(payload)
    payload.value = 2
    eq(__LOC__, MutableOverlap.value(id(value)), 2)
    payload.value = 3
    eq(__LOC__, MutableOverlap.value(id(value)), 3)
  })

  test("partial domain matches omit undefined without reading object fields", () => {
    eq(__LOC__, OptionalDomain.partial(id(OptionalDomain.Missing)), -1)
    eq(__LOC__, OptionalDomain.partial(id(OptionalDomain.Text({kind: "x", value: 2}))), 2)
    eq(__LOC__, OptionalDomain.partial(id(OptionalDomain.Number({kind: 42, reason: "number"}))), -1)
  })

  test("effect-only domain switches preserve non-tail control flow", () => {
    let result = ref("")
    eq(__LOC__, OptionalDomain.effect(id(OptionalDomain.Missing), result), "missing:done")
    eq(__LOC__, result.contents, "missing")
    eq(
      __LOC__,
      OptionalDomain.effect(id(OptionalDomain.Text({kind: "x", value: 2})), result),
      "text:2:done",
    )
    eq(__LOC__, result.contents, "text:2")
    eq(
      __LOC__,
      OptionalDomain.effect(id(OptionalDomain.Number({kind: 42, reason: "number"})), result),
      "number:done",
    )
    eq(__LOC__, result.contents, "number")
  })

  test("getter-backed discriminator fields are evaluated once", () => {
    let calls = ref(0)
    let make: (unit => unit) => Overlap.t = %raw(`touch => ({
      get kind() {touch(); return "special";}, payload: 12
    })`)
    eq(__LOC__, Overlap.partial(make(() => calls.contents = calls.contents + 1)), "special")
    eq(__LOC__, calls.contents, 1)
  })

  test("literal precedence survives reordered, partial, and guarded matches", () => {
    let value = Overlap.Broad({kind: "special", data: 12})
    eq(__LOC__, Overlap.name(value), "special:12")
    eq(__LOC__, Overlap.name(id(value)), "special:12")
    eq(__LOC__, Overlap.partial(Overlap.Broad({kind: "special", data: 12})), "special")
    eq(__LOC__, Overlap.partial(id(value)), "special")
    eq(__LOC__, Overlap.name(Overlap.externalValue()), "special:12")
    eq(__LOC__, Overlap.guarded(id(value)), "large")
    eq(__LOC__, Overlap.guarded(id(Overlap.Special({kind: SpecialKind, value: 2}))), "small")
    eq(__LOC__, Overlap.name(id(Overlap.Broad({kind: "ordinary", data: 3}))), "broad:3")
  })

  test("single inline wrapper fields use their erased generic runtime domains", () => {
    open ErasedInlineField
    eq(__LOC__, externalValues()->Array.map(name), ["text", "42", "nested", "object:3"])
    eq(__LOC__, name(Text({kind: Wrapped({value: "kind"}), text: "text"})), "text")
    eq(__LOC__, name(id(Text({kind: Wrapped({value: "kind"}), text: "text"}))), "text")
    eq(__LOC__, name(Number({kind: Wrapped({value: 42}), number: 42})), "42")
    eq(__LOC__, name(id(Number({kind: Wrapped({value: 42}), number: 42}))), "42")
    eq(
      __LOC__,
      name(Flag({kind: Nested({inner: Wrapped({value: true})}), reason: "nested"})),
      "nested",
    )
    eq(
      __LOC__,
      name(id(Flag({kind: Nested({inner: Wrapped({value: true})}), reason: "nested"}))),
      "nested",
    )
    eq(__LOC__, name(Object({kind: ObjectKind({value: "object", count: 3}), count: 3})), "object:3")
    eq(
      __LOC__,
      name(id(Object({kind: ObjectKind({value: "object", count: 3}), count: 3}))),
      "object:3",
    )
  })

  test("nested erased inline wrapper fields preserve finite literal domains", () => {
    open ErasedInlineField
    eq(__LOC__, externalLiterals()->Array.map(literalName), ["left:1", "right"])
    eq(__LOC__, literalName(Left({kind: Wrapped({value: LeftKind}), value: 1})), "left:1")
    eq(__LOC__, literalName(id(Left({kind: Wrapped({value: LeftKind}), value: 1}))), "left:1")
    eq(
      __LOC__,
      literalName(Right({kind: Nested({inner: Wrapped({value: RightKind})}), reason: "right"})),
      "right",
    )
    eq(
      __LOC__,
      literalName(id(Right({kind: Nested({inner: Wrapped({value: RightKind})}), reason: "right"}))),
      "right",
    )
  })

  test("finite repeated applications of the same erased wrapper retain scalar domains", () => {
    open ErasedInlineField
    eq(__LOC__, externalRepeated()->Array.map(repeatedName), ["repeated:1", "aliased"])
    eq(
      __LOC__,
      repeatedName(Repeated({kind: Wrapped({value: Wrapped({value: "kind"})}), value: 1})),
      "repeated:1",
    )
    eq(
      __LOC__,
      repeatedName(id(Repeated({kind: Wrapped({value: Wrapped({value: "kind"})}), value: 1}))),
      "repeated:1",
    )
    eq(
      __LOC__,
      repeatedName(Aliased({kind: Wrapped({value: Wrapped({value: 42})}), reason: "aliased"})),
      "aliased",
    )
    eq(
      __LOC__,
      repeatedName(id(Aliased({kind: Wrapped({value: Wrapped({value: 42})}), reason: "aliased"}))),
      "aliased",
    )
  })

  test("ordinary two-argument Some constructors retain their boxed field domain", () => {
    open OrdinaryOptionNames
    eq(__LOC__, externalValues()->Array.map(name), ["none:1", "some:9", "flag"])
    eq(__LOC__, name(id(Value({kind: None, value: 1}))), "none:1")
    eq(__LOC__, name(id(Value({kind: Some(2, 3), value: 4}))), "some:9")
    eq(__LOC__, name(id(Flag({kind: false, reason: "flag"}))), "flag")
  })

  test("scalar literal precedence agrees between folding and runtime dispatch", () => {
    open ScalarOverlap
    eq(__LOC__, name(Number({kind: 1, value: 2})), "one")
    eq(__LOC__, name(id(Number({kind: 1, value: 2}))), "one")
    eq(__LOC__, name(Flag({kind: true, value: 2})), "yes")
    eq(__LOC__, name(id(Flag({kind: true, value: 2}))), "yes")
    eq(__LOC__, name(Big({kind: 1n, value: 2})), "bigOne")
    eq(__LOC__, name(id(Big({kind: 1n, value: 2}))), "bigOne")
    eq(__LOC__, name(id(Number({kind: 2, value: 2}))), "number")
    eq(__LOC__, name(id(Flag({kind: false, value: 2}))), "flag")
    eq(__LOC__, name(id(Big({kind: 2n, value: 2}))), "big")
  })

  test("discrimination evaluates the matched expression and construction once", () => {
    let calls = ref(0)
    let value = Domain.Callback({
      kind: argument => argument + 1,
      argument: {
        calls.contents = calls.contents + 1
        4
      },
    })
    eq(__LOC__, calls.contents, 1)
    eq(
      __LOC__,
      Domain.name({
        calls.contents = calls.contents + 1
        id(value)
      }),
      "callback:5",
    )
    eq(__LOC__, calls.contents, 2)
    let next: (unit => unit) => Overlap.t = %raw(`effect => {
      effect(); return {kind: "special", payload: 12};
    }`)
    eq(__LOC__, Overlap.partial(next(() => calls.contents = calls.contents + 1)), "special")
    eq(__LOC__, calls.contents, 3)
  })
})
