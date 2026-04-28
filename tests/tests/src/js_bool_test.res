open Mocha
open Test_utils

let f = x =>
  if x {
    true
  } else {
    false
  }

let f2 = x =>
  if x {
    true
  } else {
    false
  }

let f4 = x =>
  if x {
    true
  } else {
    false
  }

let f3 = if true {
  true
} else {
  false
}

let u: bool = %raw(` !!1`)

let v: bool = %raw(` true`)

let ff = u =>
  if u == true {
    1
  } else {
    2
  }

let fi = (x: int, y) => x == y
let fb = (x: bool, y) => x == y
let fadd = (x: int, y) => x + y
let ffadd = (x: float, y) => x +. y

let ss = x => "xx" > x

let bb = x => (
  true > x,
  true < x,
  true >= x,
  true <= x,
  false > x,
  false < x,
  false >= x,
  false <= x,
)

let consts = (
  true && false,
  false && false,
  true && true,
  false && true,
  true || false,
  false || false,
  true || true,
  false || true,
)

let bool_array = [true, false]

describe(__MODULE__, () => {
  test("?bool_eq_caml_bool", () => eq(__LOC__, u, f(true)))
  test("js_bool_eq_js_bool", () => eq(__LOC__, v, f4(true)))
  test("js_bool_neq_acml_bool", () => ok(__LOC__, f(true) == %raw(`true`) /* not type check */))
})
