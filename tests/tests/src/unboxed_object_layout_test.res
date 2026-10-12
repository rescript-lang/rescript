open Mocha
open Test_utils

@inline(never) let id = value => value

type special = | @as("special") SpecialKind

module Named = {
  type broad = {kind: string, count: int, text: string}
  type narrow = {text: string, count: int, kind: special}
  @unboxed type t = Broad(broad) | Narrow(narrow)
  @inline(always)
  let read = value =>
    switch value {
    | Broad({count, text}) => "broad:" ++ text ++ ":" ++ Int.toString(count)
    | Narrow({text, count}) => "narrow:" ++ text ++ ":" ++ Int.toString(count)
    }
}

module Inline = {
  @unboxed
  type t =
    | Broad({
        @as("runtime-kind") kind: string,
        @as("payload-count") count: int,
        @as("payload-text") text: string,
      })
    | Narrow({
        @as("payload-text") message: string,
        @as("payload-count") amount: int,
        @as("runtime-kind") tag: special,
      })
  @inline(always)
  let read = value =>
    switch value {
    | Broad({count, text}) => "broad:" ++ text ++ ":" ++ Int.toString(count)
    | Narrow({message, amount}) => "narrow:" ++ message ++ ":" ++ Int.toString(amount)
    }
  let externalValue: unit => t = %raw(`() => ({
    "payload-text": "external", "runtime-kind": "special", "payload-count": 8
  })`)
}

module Mixed = {
  type broad = {@as("runtime-kind") tag: string, count: int, text: string}
  @unboxed
  type t = Broad(broad) | Narrow({text: string, @as("runtime-kind") kind: special, count: int})
  @inline(always)
  let read = value =>
    switch value {
    | Broad({count, text}) => "broad:" ++ text ++ ":" ++ Int.toString(count)
    | Narrow({text, count}) => "narrow:" ++ text ++ ":" ++ Int.toString(count)
    }
}

module Mutable = {
  type broad = {
    @as("runtime-kind") tag: string,
    @as("payload-count") mutable count: int,
    @as("payload-text") text: string,
  }
  @unboxed
  type t =
    | Broad(broad)
    | Narrow({
        @as("payload-text") message: string,
        @as("payload-count") mutable amount: int,
        @as("runtime-kind") kind: special,
      })
  @inline(always)
  let increment = value =>
    switch value {
    | Broad(payload) =>
      payload.count = payload.count + 1
      "broad:" ++ payload.text ++ ":" ++ Int.toString(payload.count)
    | Narrow(payload) =>
      payload.amount = payload.amount + 1
      "narrow:" ++ payload.message ++ ":" ++ Int.toString(payload.amount)
    }
}

module Gadt = {
  @unboxed
  type rec t<'a> =
    | Number({kind: int, value: int}): t<int>
    | Text({kind: string, text: string}): t<string>
  @inline(always)
  let extract:
    type a. t<a> => a =
    value =>
      switch value {
      | Number({value}) => value
      | Text({text}) => text
      }
}

describe(__MODULE__, () => {
  test("named overlap preserves fields when the literal layout reverses their order", () => {
    open Named
    eq(__LOC__, read(Broad({kind: "special", count: 7, text: "named"})), "narrow:named:7")
    eq(__LOC__, read(id(Broad({kind: "special", count: 7, text: "named"}))), "narrow:named:7")
    eq(__LOC__, read(Narrow({text: "literal", count: 9, kind: SpecialKind})), "narrow:literal:9")
    eq(
      __LOC__,
      read(id(Narrow({text: "literal", count: 9, kind: SpecialKind}))),
      "narrow:literal:9",
    )
    eq(__LOC__, read(id(Broad({kind: "ordinary", count: 3, text: "named"}))), "broad:named:3")
  })

  test("inline overlap follows renamed runtime fields through folded and runtime matches", () => {
    open Inline
    eq(__LOC__, read(Broad({kind: "special", count: 7, text: "inline"})), "narrow:inline:7")
    eq(__LOC__, read(id(Broad({kind: "special", count: 7, text: "inline"}))), "narrow:inline:7")
    eq(__LOC__, read(Narrow({message: "literal", amount: 9, tag: SpecialKind})), "narrow:literal:9")
    eq(
      __LOC__,
      read(id(Narrow({message: "literal", amount: 9, tag: SpecialKind}))),
      "narrow:literal:9",
    )
    eq(__LOC__, read(externalValue()), "narrow:external:8")
  })

  test("mixed overlap preserves a reordered discriminator and differently typed fields", () => {
    open Mixed
    eq(__LOC__, read(Broad({tag: "special", count: 7, text: "mixed"})), "narrow:mixed:7")
    eq(__LOC__, read(id(Broad({tag: "special", count: 7, text: "mixed"}))), "narrow:mixed:7")
    eq(__LOC__, read(Narrow({text: "literal", kind: SpecialKind, count: 9})), "narrow:literal:9")
    eq(
      __LOC__,
      read(id(Narrow({text: "literal", kind: SpecialKind, count: 9}))),
      "narrow:literal:9",
    )
  })

  test("folded object selection preserves reordered nonconstant payload fields", () => {
    eq(
      __LOC__,
      Named.read(Named.Broad({kind: "special", count: id(7), text: id("named")})),
      "narrow:named:7",
    )
    eq(
      __LOC__,
      Inline.read(Inline.Broad({kind: "special", count: id(7), text: id("inline")})),
      "narrow:inline:7",
    )
    eq(
      __LOC__,
      Mixed.read(Mixed.Broad({tag: "special", count: id(7), text: id("mixed")})),
      "narrow:mixed:7",
    )
  })

  test("reordered mutable literal payloads update the original broad record alias", () => {
    open Mutable
    let payload: broad = {tag: "special", count: 7, text: "mutable"}
    eq(__LOC__, increment(id(Broad(payload))), "narrow:mutable:8")
    eq(__LOC__, payload.count, 8)
    eq(__LOC__, payload.text, "mutable")
    eq(__LOC__, increment(Broad(payload)), "narrow:mutable:9")
    eq(__LOC__, payload.count, 9)
    eq(__LOC__, increment(Broad({tag: "special", count: 1, text: "folded"})), "narrow:folded:2")
    eq(
      __LOC__,
      increment(id(Broad({tag: "special", count: 1, text: "runtime"}))),
      "narrow:runtime:2",
    )
  })

  test("disjoint object GADT domains retain their result type refinements", () => {
    open Gadt
    let number: int = extract(Number({kind: 1, value: 42}))
    let dynamicNumber: int = extract(id(Number({kind: 1, value: 42})))
    let text: string = extract(Text({kind: "text", text: "result"}))
    let dynamicText: string = extract(id(Text({kind: "text", text: "result"})))
    eq(__LOC__, number, 42)
    eq(__LOC__, dynamicNumber, 42)
    eq(__LOC__, text, "result")
    eq(__LOC__, dynamicText, "result")
  })
})
