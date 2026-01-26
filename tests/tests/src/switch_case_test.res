open Mocha
open Test_utils

let f = x =>
  switch x {
  | "xx'''" => 0
  | "xx\"" => 1
  | `xx\\"` => 2
  | `xx\\""` => 3
  | _ => 4
  }

describe(__MODULE__, () => {
  test("switch case with escaped strings", () => {
    eq(__LOC__, f("xx'''"), 0)
    eq(__LOC__, f("xx\""), 1)
    eq(__LOC__, f(`xx\\"`), 2)
    eq(__LOC__, f(`xx\\""`), 3)
  })
})
