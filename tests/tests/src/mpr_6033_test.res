open Mocha
open Test_utils

let f = x =>
  switch x {
  | y => Lazy.get(y) ++ "abc"
  }

describe(__MODULE__, () => {
  test("lazy evaluation", () => {
    let x = Lazy.from_fun(() => "def")
    ignore(Lazy.get(x))
    let u = f(x)
    eq(__LOC__, u, "defabc")
  })
})
