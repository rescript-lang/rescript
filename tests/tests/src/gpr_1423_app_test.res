open Mocha
open Test_utils

let foo = f => Js.log(f(~a1="a1", ()))

let _ = foo(Gpr_1423_nav.busted(~a2="a2", ...))

let foo2 = f => f(~a1="a1", ())

describe(__MODULE__, () => {
  test("gpr_1423_app_test", () => eq(__LOC__, foo2(Gpr_1423_nav.busted(~a2="a2", ...)), "a1a2"))
})
