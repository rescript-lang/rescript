open Mocha
open Test_utils

// The partial applications inline the local functions in JS: constant
// arguments replace the parameters, except the string, which stays bound.
let scaled = {
  let scale = (k, offset, flip, label, x, y) => {
    let a = x *. k +. offset
    let b = y *. k -. offset
    let c = if flip {
      b -. a
    } else {
      a -. b
    }
    label ++ Float.toString(c +. a *. b)
  }
  scale(2.5, -1., true, "value: ", 1., ...)
}

let intsApplied = {
  let ints = (a, b, big, x, y) => {
    let s = x + a * y - b
    let t = s * a + b * x
    big + BigInt.fromInt(s * t + a)
  }
  ints(-3, 7, 10n, 2, ...)
}

// [n] is reassigned by the loop, so the constant must stay bound.
let count = () => {
  let rec loop = (n, acc) =>
    if n > 0 {
      loop(n - 1, acc + n)
    } else {
      acc
    }
  loop(10, 0)
}

describe(__MODULE__, () => {
  test("float, bool and string constants", () => {
    eq(__LOC__, "value: 13.5", scaled(2.))
  })
  test("int and bigint constants", () => {
    eq(__LOC__, -777n, intsApplied(3))
  })
  test("reassigned parameter", () => {
    eq(__LOC__, 55, count())
  })
})
