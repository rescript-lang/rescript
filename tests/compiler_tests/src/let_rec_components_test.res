open Mocha
open Test_utils

// One `let rec` group that the compiler splits into components: a mutual
// recursion, a function and a tuple that only use it, a self-recursive
// function, and bindings that refer to nothing else in the group.
let rec isEven = n => n == 0 ? true : isOdd(n - 1)
and isOdd = n => n == 0 ? false : isEven(n - 1)
and parity = n => isEven(n) ? "even" : "odd"
and helpers = (isOdd, offset)
and countdown = n => n <= 0 ? list{} : list{n, ...countdown(n - 1)}
and offset = 10
and shifted = n => n + offset
and describe' = n => parity(n) ++ " " ++ Int.toString(shifted(n))

describe(__MODULE__, () => {
  test("mutual recursion", () => {
    eq(__LOC__, true, isEven(10))
    eq(__LOC__, true, isOdd(7))
  })
  test("users of the cycle", () => {
    eq(__LOC__, "odd", parity(3))
    let (odd, base) = helpers
    eq(__LOC__, true, odd(7))
    eq(__LOC__, 10, base)
  })
  test("self recursion", () => eq(__LOC__, list{3, 2, 1}, countdown(3)))
  test("bindings that use other components", () => {
    eq(__LOC__, 15, shifted(5))
    eq(__LOC__, "even 14", describe'(4))
  })
})
