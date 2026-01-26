open Mocha
open Test_utils

type t = {
  a0: int,
  a1: int,
  a2: int,
  a3: int,
  a4: int,
  a5: int,
  /* a6 : int ; */
  /* mutable a7 : int ; */
}

type invalidRecord = {invalid_js_id': int, x: int}

let f = (x: t) => {
  let y: t = Obj.magic(Obj.dup(Obj.repr(x)))
  {...y, a0: 1}
}

describe(__MODULE__, () => {
  test("record update operations", () => {
    let v = {a0: 0, a1: 0, a2: 0, a3: 0, a4: 0, a5: 0}
    eq(__LOC__, v.a0 + 1, f(v).a0)
  })

  test("record update with invalid js id", () => {
    let val0: invalidRecord = {invalid_js_id': 3, x: 2}
    let fff = (x: invalidRecord) => {...x, invalid_js_id': x.invalid_js_id' + 2}
    let val1 = fff(val0)
    eq(__LOC__, val0.invalid_js_id', 3)
    eq(__LOC__, val1.invalid_js_id', 5)
  })
})
