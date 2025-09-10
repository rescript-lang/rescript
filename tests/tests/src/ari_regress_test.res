open Mocha
open Test_utils

let f = x => \"+"(x, ...)
let g = f(3)(4)

let h = ref(0)

let gg = (x, y) => {
  let u = x + y
  z => u + z
}

let g1 = (x, y) => {
  let u = x + y
  let () = incr(h)
  (xx, yy) => xx + yy + u
}
let x = gg(3, 5)(6)

let v = g1(3, 4)(6, _)

describe(__MODULE__, () => {
  test("curry", () => {
    eq(__LOC__, g, 7)
  })

  test("curry2", () => {
    eq(
      __LOC__,
      14,
      {
        ignore(v(1))
        v(1)
      },
    )
  })

  test("curry3", () => {
    eq(__LOC__, x, 14)
  })

  test("ref count", () => {
    eq(__LOC__, h.contents, 2)
  })
})
