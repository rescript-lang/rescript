open Mocha
open Test_utils

let $$ = (x, y) => x + y

let v = $$(1, 2)

let \"$$+" = (x, y) => x * y

let u = \"$$+"(1, 3)

// The shadowing `$` must not be printed as `$$1`, the parameter's name.
let f = ($, $$1) => {
  let $ = $$1($)
  let x = $$1(2)
  `$: ${$} ${x}`
}

describe(__MODULE__, () => {
  test("dollar escape operators", () => {
    eq(__LOC__, v, 3)
    eq(__LOC__, u, 3)
  })

  test("dollar identifiers shadowing", () => {
    eq(__LOC__, f(1, n => Int.toString(n * 10)), "$: 10 20")
  })
})
