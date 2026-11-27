// Inlining a call can inline the functions that the inlined body calls, but
// only a few levels deep: in a chain of small functions that each call the
// previous one, a later function calls an earlier one instead of containing
// the whole chain. Functions marked @inline are still inlined all the way.

let f0 = x => x + 1
let f1 = x => f0(x) + 1
let f2 = x => f1(x) + 2
let f3 = x => f2(x) + 3
let f4 = x => f3(x) + 4
let f5 = x => f4(x) + 5
let f6 = x => f5(x) + 6

@inline let g0 = x => x * 2
@inline let g1 = x => g0(x) * 2
@inline let g2 = x => g1(x) * 2
@inline let g3 = x => g2(x) * 2
let g4 = x => g3(x) + 1

// Read through a ref so the calls below are not evaluated at compile time
let zero = ref(0)

open Mocha
open Test_utils

describe(__MODULE__, () => {
  test("chain of small functions", () => {
    eq(__LOC__, f6(zero.contents), 22)
    eq(__LOC__, f3(zero.contents + 1), 8)
  })
  test("chain of @inline functions", () => {
    eq(__LOC__, g4(zero.contents + 1), 17)
  })
})
