open Mocha
open Test_utils

// `Nullable.isNullable(e)` is printed as `e == null`, which binds like `===`:
// `e` keeps the parentheses it needs, and the check needs none as an operand
// of `||` or as the test of `?:`.

// Hides values from the optimizer, so the calls below are not folded away
let opaque: 'a => 'a = %raw(`x => x`)

let ofCond = (c, a: Nullable.t<int>, b: Nullable.t<int>) => Nullable.isNullable(c ? a : b)

let ofIf = (c, a: Nullable.t<int>, b: Nullable.t<int>) =>
  Nullable.isNullable(
    if c {
      a
    } else {
      b
    },
  )

let ofOr = (a: bool, b: bool) => Nullable.isNullable(Nullable.make(a || b))
let ofSum = (a: int, b: int) => Nullable.isNullable(Nullable.make(a + b))
let ofObject = (a: int) => () => Nullable.isNullable(Nullable.make({"a": a}))
let inOr = (x: Nullable.t<int>, y: bool) => Nullable.isNullable(x) || y
let inCond = (x: Nullable.t<int>) => Nullable.isNullable(x) ? "none" : "some"
let inEq = (x: Nullable.t<int>, y: bool) => y == Nullable.isNullable(x)

describe(__MODULE__, () => {
  test("isNullable of a conditional", () => {
    let (t, f) = (opaque(true), opaque(false))
    let (null, one) = (opaque(Nullable.null), opaque(Nullable.make(1)))
    eq(__LOC__, ofCond(t, null, one), true)
    eq(__LOC__, ofCond(t, one, null), false)
    eq(__LOC__, ofCond(f, one, opaque(Nullable.undefined)), true)
    eq(__LOC__, ofIf(t, null, one), true)
    eq(__LOC__, ofIf(f, null, one), false)
  })

  test("isNullable of a logical or", () => eq(__LOC__, ofOr(opaque(true), opaque(false)), false))

  test("isNullable of an int sum", () => eq(__LOC__, ofSum(opaque(1), 2), false))

  test("isNullable of an object in an arrow body", () => eq(__LOC__, ofObject(opaque(1))(), false))

  test("isNullable as an operand", () => {
    let (null, one) = (opaque(Nullable.null), opaque(Nullable.make(1)))
    eq(__LOC__, inOr(null, false), true)
    eq(__LOC__, inOr(one, false), false)
    eq(__LOC__, inCond(null), "none")
    eq(__LOC__, inCond(one), "some")
    eq(__LOC__, inEq(null, true), true)
    eq(__LOC__, inEq(one, true), false)
  })
})
