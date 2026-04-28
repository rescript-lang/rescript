open Mocha
open Test_utils

let f = () => {
  let v = ref(0)
  let acc = ref(0)

  let rec loop = (n): int =>
    if v.contents > n {
      acc.contents
    } else {
      acc := acc.contents + v.contents
      incr(v)
      loop(n)
    }
  loop(10)
}

describe(__MODULE__, () => {
  test("sum", () => {
    eq(__LOC__, 55, f())
  })
})
