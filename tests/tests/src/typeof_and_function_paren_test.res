open Mocha
open Test_utils

// `typeof` and a function take parentheses as the object of a member access
// or a call, and a function as an operand.

@get external arity: ('a => 'b) => int = "length"
@send external call: ('a => 'b, Nullable.t<'s>, 'a) => 'b = "call"
@get external arityThis: (@this ('self, int) => int) => int = "length"
@send external callThis: (@this ('self, int) => int, Nullable.t<'s>, int) => int = "call"

let x: unknown = %raw(`"abc"`)

let typeofLength = () => String.length(typeof(x) :> string)
let typeofUpper = () => (typeof(x) :> string)->String.toUpperCase

let called = ref(0)

// The call is a statement, which a function expression must not start
let callThisStatement = () => {
  callThis(@this (_, n) => {
    called := n
    n
  }, Nullable.null, 3)->ignore
  called.contents
}

describe(__MODULE__, () => {
  test("typeof as the object", () => {
    eq(__LOC__, typeofLength(), 6)
    eq(__LOC__, typeofUpper(), "STRING")
  })

  test("arrow function as the object", () => {
    eq(__LOC__, arity(n => n + 1), 1)
    eq(__LOC__, arity(async n => n + 1), 1)
    eq(__LOC__, call(n => n * 2, Nullable.null, 21), 42)
  })

  test("function expression as the object", () => {
    eq(__LOC__, arityThis(@this (_, n) => n + 1), 1)
    eq(__LOC__, callThisStatement(), 3)
  })

  test("arrow function as an operand", () => eq(__LOC__, (n => n + 1) === (n => n + 1), false))
})
