open Mocha
open Test_utils

module H = Inline_const.N1()

let (f, f1, f2, f3, f4, f5, f6, f7) = {
  open Inline_const
  (f, f1, f2, N.f3, H.f4, f5, f6, H.xx)
}

describe(__LOC__, () => {
  test("inline const test", () => {
    eq(__LOC__, f, "hello")
    eq(__LOC__, f1, "a")
    eq(__LOC__, f2, `中文`)
    eq(__LOC__, f3, `中文`)
    eq(__LOC__, f4, `中文`)
    eq(__LOC__, f5, true)
    eq(__LOC__, f6, 1)
    eq(__LOC__, f7, 0.000003)
  })
})
