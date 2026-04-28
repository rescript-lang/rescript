let rec f = (b, x, n) =>
  if n > 100000 {
    false
  } else {
    b && f(b, x, n + 1)
  }

let rec or_f = (b, x, n) =>
  if n > 100000 {
    false
  } else {
    b || or_f(b, x, n + 1)
  }

open Mocha
open Test_utils

describe(__MODULE__, () => {
  /* becareful inlining will defeat the test purpose here */
  test("and_tail", () => {
    eq(__LOC__, false, f(true, 1, 0))
  })
  test("or_tail", () => {
    eq(__LOC__, false, or_f(false, 1, 0))
  })
})
