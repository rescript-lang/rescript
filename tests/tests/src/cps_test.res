open Mocha
open Test_utils

let test_sum = () => {
  let v = ref(0)
  let rec f = (n, acc) =>
    if n == 0 {
      acc()
    } else {
      f(n - 1, _ => {
        v := v.contents + n
        acc()
      })
    }
  f(10, _ => ())
  v.contents
}

let test_closure = () => {
  let n = 6
  let v = ref(0)
  let arr = Belt.Array.make(n, x => x)
  for i in 0 to n - 1 {
    arr[i] = _ => i
  }
  arr->Belt.Array.forEach(i => v := v.contents + i(0))
  v.contents
}

let test_closure2 = () => {
  let n = 6
  let v = ref(0)
  let arr = Belt.Array.make(n, x => x)
  for i in 0 to n - 1 {
    let j = i + i
    arr[i] = _ => j
  }
  arr->Belt.Array.forEach(i => v := v.contents + i(0))
  v.contents
}

describe(__MODULE__, () => {
  test("cps_test_sum", () => eq(__LOC__, 55, test_sum()))
  test("cps_test_closure", () => eq(__LOC__, 15, test_closure()))
  test("cps_test_closure2", () => eq(__LOC__, 30, test_closure2()))
})
