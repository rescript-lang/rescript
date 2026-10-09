open Mocha
open Test_utils

type nested<'a> = array<option<array<'a>>>

let make = (x): nested<int> => [Some([x])]
let operators = (a, b) => (a >> b, a >>> b, a << b, a >= b, a <= b)
let precedence = (a, b, c) => a >> b + 1 >= c && a << b + 1 == 128
let template = (a, b) => `shift ${Int.toString(a >> b)}`

let nestedTemplate = x => `outer ${`inner ${x}`} }tail`
let dotRegex = /.foo/g

describe("parser token cursor", () => {
  test("signed, unsigned, left shift and comparisons", () => {
    eq(__LOC__, operators(-8, 1), (-4, 2147483644, -16, false, true))
  })
  test("precedence and evaluation through Lambda", () => {
    eq(__LOC__, precedence(32, 1, 8), true)
    eq(__LOC__, precedence(32, 1, 9), false)
  })
  test("regex prefixes and nested raw template chunks", () => {
    eq(__LOC__, RegExp.source(dotRegex), ".foo")
    eq(__LOC__, nestedTemplate("💎"), "outer inner 💎 }tail")
  })
  test("nested type arguments and template interpolation", () => {
    eq(__LOC__, make(8), [Some([8])])
    eq(__LOC__, template(8, 1), "shift 4")
  })
})
