let f = () => {
  let rec f = (acc, n) =>
    if n > 0 {
      f(acc + n, n - 1)
    } else {
      acc
    }
  let len = 10
  let v = Belt.Array.make(len, 0)
  for i in 0 to len - 1 {
    v[i] = f(0, i)
  }
  v
}

open Mocha
open Test_utils

describe(__MODULE__, () => {
  test("acc", () => {
    eq(__LOC__, [0, 1, 3, 6, 10, 15, 21, 28, 36, 45], f())
  })
})
