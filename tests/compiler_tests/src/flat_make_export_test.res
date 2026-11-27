open Mocha
open Test_utils

@module("mocha")
external testAsync: (string, unit => promise<unit>) => unit = "test"

describe(__MODULE__, () => {
  test("nested make values are read through flat exports", () => {
    eq(__LOC__, Flat_make_export_use.top, "top")
    eq(__LOC__, Flat_make_export_use.one, "one")
    eq(__LOC__, Flat_make_export_use.two, "two")
    eq(__LOC__, Flat_make_export_use.three, "three")
  })

  test("other values keep their module path", () => {
    eq(__LOC__, Flat_make_export_use.oneKeep, "one-keep")
    eq(__LOC__, Flat_make_export_use.twoKeep, "two-inner-keep")
    eq(__LOC__, Flat_make_export_use.threeKeep, "three-deep-keep")
    eq(__LOC__, Flat_make_export_use.exoticValue, "exotic-value")
  })

  test("flat names follow the signature, not the implementation", () => {
    eq(__LOC__, Flat_make_export_use.nested, "nested")
    eq(__LOC__, Flat_make_export_use.recursive, "recursive")
    eq(__LOC__, Flat_make_export_use.coerced, "coerced")
    eq(__LOC__, Flat_make_export_use.rebound->Option.map(f => f()), Some("rebound"))
    eq(__LOC__, Flat_make_export_use.applied, "applied")
    eq(__LOC__, Flat_make_export_use.included, "one")
    eq(__LOC__, Flat_make_export_use.aliased, "one")
  })

  test("re-exported modules read the flat exports", () => {
    eq(__LOC__, Flat_make_export_use.reexported, "two")
    eq(__LOC__, Flat_make_export_use.reexportedDeep, "three")
  })

  testAsync("awaited modules read the flat exports", async () => {
    eq(__LOC__, await Flat_make_export_use.awaited(), ("two", "three"))
  })

  test("a binding hidden by the interface does not clash", () => {
    eq(__LOC__, Flat_make_export_use.hidden, 1)
    eq(__LOC__, Flat_make_export_use.hiddenBoth, 3)
  })
})
