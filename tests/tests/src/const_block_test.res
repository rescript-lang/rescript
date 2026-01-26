open Mocha
open Test_utils

let a = [0., 1., 2.]
let b = [0, 1, 2]
let c = [0, 1, 2, 3, 4, 5]
let v = (0, 1, 2, 3, 4, 5)
let f = () => {
  a->Array.setUnsafe(0, 3.0)
  b->Array.setUnsafe(0, 3)
}

/** should not be inlined here 
        everytime we call [h ()], 
        it should share the same copy
     */
let h = () => c

/** should not be inlined here 
        everytime we call [h ()], 
        it should share the same copy
     */
let g = () => {
  f()
  eq(__LOC__, (a->Array.getUnsafe(0), b->Array.getUnsafe(0)), (3.0, 3))
}

describe(__MODULE__, () => {
  test("const_block_test", () => {
    g()
  })

  test("avoid_mutable_inline_test", () => {
    let v = h()
    let v2 = h()
    let () = {
      v->Array.setUnsafe(0, 3)
      v2->Array.setUnsafe(1, 4)
    }
    eq(__LOC__, [3, 4, 2, 3, 4, 5], v)
  })
})
