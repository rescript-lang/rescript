open Mocha
open Test_utils

@module("mocha")
external testAsync: (string, unit => promise<unit>) => unit = "test"

// Negating a float gives `-0` for `0`, subtracting it from zero gives `0`.
// `**` is parenthesized as the operand of a prefix operator.

external floatAsBool: float => bool = "%identity"
external floatAsInt: float => int = "%identity"
external floatOfSome: option<float> => float = "%identity"

@inline(never) let id = x => x

// Printed as its payload
@unboxed type floatOrString = Float(float) | String(string)

let zero = id(0.)
let base = id(2.)
let exponent = id(3.)

@inline(never) let subtractFrom = (start, x) => Float.fromInt(start) -. x

let rec countdown = (start, x) =>
  if x > 0. {
    countdown(0, x -. 1.)
  } else {
    Float.fromInt(start) -. x
  }

// The literal is inlined into the negation.
let negateFiveAfter = steps => {
  let rec negateAfter = (x, steps) =>
    if steps > 0 {
      negateAfter(x, steps - 1)
    } else {
      -x
    }
  negateAfter(-5, steps)
}

let isNegativeZero = x => 1. /. x === Float.Constants.negativeInfinity

describe(__MODULE__, () => {
  test("subtraction from a zero float literal", () => {
    eq(__LOC__, isNegativeZero(0. -. zero), false)
    eq(__LOC__, 0. -. base, -2.)
    eq(__LOC__, 0. -. base ** exponent, -8.)
  })

  test("subtraction from a converted zero int", () => {
    eq(__LOC__, isNegativeZero(Float.fromInt(0) -. zero), false)
    eq(__LOC__, isNegativeZero(subtractFrom(0, zero)), false)
    eq(__LOC__, isNegativeZero(countdown(0, zero)), false)
  })

  test("subtraction from a negative zero literal", () => {
    eq(__LOC__, isNegativeZero(-0. -. zero), true)
    eq(__LOC__, isNegativeZero(-0. -. id(-0.)), false)
  })

  test("negation", () => {
    eq(__LOC__, isNegativeZero(-.zero), true)
    eq(__LOC__, isNegativeZero(-.(-.zero)), false)
    eq(__LOC__, -.base, -2.)
    eq(__LOC__, -.base ** exponent, -8.)
  })

  test("negated int", () => {
    let n = id(0)
    eq(__LOC__, isNegativeZero(Int.toFloat(-n)), false)
    eq(__LOC__, -id(5) ||| id(0), -5)
    eq(__LOC__, -(id(2) ** id(3)), -8)
  })

  test("negated int as the left operand of a bitwise operator", () => {
    eq(__LOC__, -id(5) &&& 3, 3)
    eq(__LOC__, -id(5) ||| 2, -5)
    eq(__LOC__, -id(5) ^^^ 2, -7)
    eq(__LOC__, -id(5) << 1, -10)
    eq(__LOC__, -id(5) >> 1, -3)
    eq(__LOC__, -id(5) >>> 28, 15)
  })

  test("negated int as the right operand of a bitwise operator", () => {
    eq(__LOC__, 3 &&& -id(5), 3)
    eq(__LOC__, 2 ||| -id(5), -5)
    eq(__LOC__, 2 ^^^ -id(5), -7)
    eq(__LOC__, 1 << -id(-3), 8)
    eq(__LOC__, -16 >> -id(-3), -2)
    eq(__LOC__, 16 >>> -id(-3), 2)
  })

  test("subtraction of a negative from a converted zero int", () => {
    let five = id(5.)
    eq(__LOC__, Float.toInt(Float.fromInt(0) -. -.five), 5)
    eq(__LOC__, Float.toInt(Float.fromInt(0) -. -.(base ** exponent)), 8)
    eq(__LOC__, Float.toInt(Float.fromInt(0) -. -5.), 5)
    eq(__LOC__, Float.toInt(Float.fromInt(0) -. floatOfSome(Some(-.five))), 5)
  })

  test("negation of an inlined negative int literal", () => {
    eq(__LOC__, negateFiveAfter(id(2)), 5)
  })

  // The `-` of an int negation is kept apart from an operand that prints
  // with a leading minus itself, as `--` is a decrement or a syntax error.
  test("int negation of an operand printed with a leading minus", () => {
    eq(__LOC__, -(%raw("-5"): int), 5)
    eq(__LOC__, -Option.getUnsafe(Some((%raw("-5"): int))), 5)
    eq(__LOC__, 3 &&& -(%raw("-5"): int), 1)
    eq(__LOC__, -(Obj.magic(-5.5): int), 5)
    eq(__LOC__, -(Obj.magic(-0.): int), 0)
    eq(__LOC__, -floatAsInt(-.id(5.)), 5)
    eq(__LOC__, -(Obj.magic(Float(-5.)): int), 5)
  })

  // `- -5n | 0` parses, and then throws as it mixes a bigint and an int.
  test("int negation of a negative bigint", () => {
    let error = try {
      ignore(id(-(Obj.magic(-5n): int)))
      None
    } catch {
    | JsExn(e) => JsExn.name(e)
    }
    eq(__LOC__, error, Some("TypeError"))
  })

  // The conversions are erased, so the subtraction itself is the operand.
  test("subtraction from a converted zero int under a bitwise operator", () => {
    let negated = x => floatAsInt(Float.fromInt(0) -. x)
    let five = id(5.)
    let minusThree = id(-3.)
    eq(__LOC__, negated(five) &&& 3, 3)
    eq(__LOC__, negated(five) ||| 2, -5)
    eq(__LOC__, negated(five) ^^^ 2, -7)
    eq(__LOC__, negated(five) << 1, -10)
    eq(__LOC__, negated(five) >> 1, -3)
    eq(__LOC__, negated(five) >>> 28, 15)
    eq(__LOC__, 3 &&& negated(five), 3)
    eq(__LOC__, 2 ||| negated(five), -5)
    eq(__LOC__, 2 ^^^ negated(five), -7)
    eq(__LOC__, 1 << negated(minusThree), 8)
    eq(__LOC__, -16 >> negated(minusThree), -2)
    eq(__LOC__, 16 >>> negated(minusThree), 2)
    eq(__LOC__, 3 &&& negated(base ** exponent), 0)
  })

  test("prefix operators on a power", () => {
    eq(__LOC__, -.(base ** exponent), -8.)
    eq(__LOC__, typeof(base ** exponent), #number)
    eq(__LOC__, typeof(Some(base ** exponent)), #number)
    eq(__LOC__, ~~~(id(2) ** id(3)), -9)
    eq(__LOC__, ~~~(id(2n) ** id(3n)), -9n)
    eq(__LOC__, !floatAsBool(base ** exponent), false)
    eq(__LOC__, -.(Obj.magic(Float(base ** exponent)): float), -8.)
    eq(__LOC__, typeof(Float(base ** exponent)), #number)
  })

  test("prefix operators on operators other than a power", () => {
    eq(__LOC__, typeof(base *. exponent), #number)
    eq(__LOC__, -.base *. exponent, -6.)
    eq(__LOC__, -.(base *. exponent), -6.)
    eq(__LOC__, ~~~floatAsInt(-.base), 1)
    eq(__LOC__, ~~~(~~~id(2n)), 2n)
    eq(__LOC__, !floatAsBool(-.base), false)
  })

  testAsync("await on a power", async () => {
    let awaitPow = async (a: float, b: float) => await Obj.magic(a ** b)
    eq(__LOC__, await awaitPow(base, exponent), 8.)
  })
})
