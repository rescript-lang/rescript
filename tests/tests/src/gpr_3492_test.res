open Mocha
open Test_utils

%%raw("function foo(a){return a()}")

@val("foo") external foo: (unit => int) => int = ""

let fn = () => {
  Js.log("hi")
  1
}

describe(__MODULE__, () => {
  test("external function call", () => {
    eq(__LOC__, foo(fn), 1)
  })
})
