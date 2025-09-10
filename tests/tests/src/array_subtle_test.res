open Mocha
open Test_utils

module Array = Ocaml_Array

let v = [1, 2, 3, 3]

let f = v => {
  switch Js.Array2.pop(v) {
  | Some(x) => Js.log("hi")
  | None => Js.log("hi2")
  }
  Js.log(ignore(Js.Array2.pop(v)))
}

let fff = x => Array.length(x) >= 0

let fff2 = x =>
  if Array.length(x) >= 10 {
    Js.log("hi")
  }

let fff3 = x =>
  if Array.length(x) >= 0 {
    1
  } else {
    2
  }

let fff4 = x =>
  if Array.length(x) > 0 {
    1
  } else {
    2
  }

describe(__MODULE__, () => {
  test("array_length_test", () => {
    let v = [1, 2, 3, 3]
    eq(__LOC__, 4, Array.length(v))
  })

  test("array_push_test", () => {
    let v = [1, 2, 3, 3]
    eq(__LOC__, 5, Js.Array2.push(v, 3))
    eq(__LOC__, 5, Array.length(v))
    eq(__LOC__, 5, Js.Array2.length(v))
  })

  test("array_mutation_test", () => {
    let v = [1, 2, 3, 3]
    eq(__LOC__, 3, v[2])
    v[2] = 4
    eq(__LOC__, 4, v[2])
  })

  test("array_pop_test", () => {
    let v = [1, 2, 3, 3]
    while Js.Array2.length(v) > 0 {
      ignore(Js.Array2.pop(v))
    }
    eq(__LOC__, 0, Js.Array2.length(v))
  })

  test("array_function_tests", () => {
    eq(__LOC__, 1, fff3([]))
    eq(__LOC__, 2, fff4([]))
    eq(__LOC__, 1, fff4([1]))
  })
})
