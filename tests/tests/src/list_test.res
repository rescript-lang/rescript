open Belt
open Mocha
open Test_utils

let intEq = (a, b) => a == b

describe(__MODULE__, () => {
  test("length", () => {
    eq(__LOC__, 1, List.length(list{(0, 1, 2, 3, 4)})) /* This is tuple haha */
  })

  test("length2", () => {
    eq(__LOC__, 5, List.length(list{0, 1, 2, 3, 4})) /* This is tuple haha */
  })

  test("long_length", () => {
    let v = 30_000
    eq(__LOC__, v, List.length(List.fromArray(Array.init(v, _ => 0))))
  })

  test("sort", () => {
    eq(
      __LOC__,
      list{4, 1, 2, 3}->List.sort((x: int, y) => Pervasives.compare(x, y)),
      list{1, 2, 3, 4},
    )
  })

  test("has true", () => {
    eq(__LOC__, true, List.has(list{1, 2, 3}, 3, intEq))
  })

  test("has false", () => {
    eq(__LOC__, false, List.has(list{1, 2, 3}, 4, intEq))
  })

  test("getAssoc", () => {
    eq(__LOC__, Some(9), List.getAssoc(list{(1, 2), (4, 9)}, 4, intEq))
  })
})
