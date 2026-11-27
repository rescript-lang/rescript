open Mocha
open Test_utils

type rec a = {b: b}
and b = {a: a}

let rec a = {b: b} and b = {a: a}

let is_inifite = (x: a) => x.b.a === x

describe(__MODULE__, () => {
  test("recursive type infinite loop", () => {
    eq(__LOC__, true, is_inifite(a))
  })
})
