open Mocha
open Test_utils

let u = ((a, b) => a + b)(1, 2)

let nullary = () => 3

let unary = a => a + 3

let xx = unary(3)
test("ppx_apply_test_unary", () => {
  eq(__LOC__, u, 3)
})

@val external f: int => int = "xx"

let h = a => f(a)

describe(__MODULE__, () => {
  test("function_application_test", () => {
    let u = ((a, b) => a + b)(1, 2)
    let nullary = () => 3
    let unary = a => a + 3
    let xx = unary(3)
    eq(__LOC__, u, 3)
  })

  test("external_function_test", () => {
    let h = a => f(a)
    ok(__LOC__, true)
  })
})
