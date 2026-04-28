open Mocha
open Test_utils

let simpleEq = (a: int, b) => a == b

describe(__MODULE__, () => {
  test("option_isSome_Some", () => {
    eq(__LOC__, true, Js.Option.isSome(Some(1)))
  })

  test("option_isSome_None", () => {
    eq(__LOC__, false, Js.Option.isSome(None))
  })

  test("option_isNone_Some", () => {
    eq(__LOC__, false, Js.Option.isNone(Some(1)))
  })

  test("option_isNone_None", () => {
    eq(__LOC__, true, Js.Option.isNone(None))
  })

  test("option_isSomeValue_Eq", () => {
    eq(__LOC__, true, Js.Option.isSomeValue(simpleEq, 2, Some(2)))
  })

  test("option_isSomeValue_Diff", () => {
    eq(__LOC__, false, Js.Option.isSomeValue(simpleEq, 1, Some(2)))
  })

  test("option_isSomeValue_DiffNone", () => {
    eq(__LOC__, false, Js.Option.isSomeValue(simpleEq, 1, None))
  })

  test("option_getExn_Some", () => {
    eq(__LOC__, 2, Js.Option.getExn(Some(2)))
  })

  test("option_equal_Eq", () => {
    eq(__LOC__, true, Js.Option.equal(simpleEq, Some(2), Some(2)))
  })

  test("option_equal_Diff", () => {
    eq(__LOC__, false, Js.Option.equal(simpleEq, Some(1), Some(2)))
  })

  test("option_equal_DiffNone", () => {
    eq(__LOC__, false, Js.Option.equal(simpleEq, Some(1), None))
  })

  test("option_andThen_SomeSome", () => {
    eq(
      __LOC__,
      true,
      Js.Option.isSomeValue(simpleEq, 3, Js.Option.andThen(a => Some(a + 1), Some(2))),
    )
  })

  test("option_andThen_SomeNone", () => {
    eq(__LOC__, false, Js.Option.isSomeValue(simpleEq, 3, Js.Option.andThen(_ => None, Some(2))))
  })

  test("option_map_Some", () => {
    eq(__LOC__, true, Js.Option.isSomeValue(simpleEq, 3, Js.Option.map(a => a + 1, Some(2))))
  })

  test("option_map_None", () => {
    eq(__LOC__, None, Js.Option.map(a => a + 1, None))
  })

  test("option_default_Some", () => {
    eq(__LOC__, 2, Js.Option.getWithDefault(3, Some(2)))
  })

  test("option_default_None", () => {
    eq(__LOC__, 3, Js.Option.getWithDefault(3, None))
  })

  test("option_filter_Pass", () => {
    eq(
      __LOC__,
      true,
      Js.Option.isSomeValue(simpleEq, 2, Js.Option.filter(a => mod(a, 2) == 0, Some(2))),
    )
  })

  test("option_filter_Reject", () => {
    eq(__LOC__, None, Js.Option.filter(a => mod(a, 3) == 0, Some(2)))
  })

  test("option_filter_None", () => {
    eq(__LOC__, None, Js.Option.filter(a => mod(a, 3) == 0, None))
  })

  test("option_firstSome_First", () => {
    eq(__LOC__, true, Js.Option.isSomeValue(simpleEq, 3, Js.Option.firstSome(Some(3), Some(2))))
  })

  test("option_firstSome_Second", () => {
    eq(__LOC__, true, Js.Option.isSomeValue(simpleEq, 2, Js.Option.firstSome(None, Some(2))))
  })

  test("option_firstSome_None", () => {
    eq(__LOC__, None, Js.Option.firstSome(None, None))
  })
})
