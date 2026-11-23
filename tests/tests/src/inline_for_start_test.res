open Mocha
open Test_utils

// Applying a function literal right away inlines it, which renames the
// variables bound in its body, including those in a for loop's start
// expression: a let in a block, and a callback's parameters.
let fromBlock = x =>
  (
    n => {
      let acc = ref(0)
      for i in {
        let s = n * 2
        s - n
      } to n + 3 {
        acc := acc.contents + i
      }
      acc.contents
    }
  )(x)

let fromCallback = (xs, x) =>
  (
    n => {
      let acc = ref(0)
      for i in xs->Array.reduce(0, (a, b) => a + b) to n {
        acc := acc.contents + i
      }
      acc.contents
    }
  )(x)

describe(__MODULE__, () => {
  test("a let in the start expression", () => {
    // 2 + 3 + 4 + 5
    eq(__LOC__, fromBlock(2), 14)
  })
  test("a callback in the start expression", () => {
    // 3 + 4 + 5
    eq(__LOC__, fromCallback([1, 2], 5), 12)
  })
})
