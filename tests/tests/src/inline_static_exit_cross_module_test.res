@@config({flags: ["-bs-cross-module-opt"]})

// Two copies of another module's function, or of its functor's body, each have
// their own static exits, so each runs its own handler.
open Mocha
open Test_utils

@inline(never) let id = x => x

let constant = Inline_static_exit_cross_module_lib.pick(0, 5, true)
let computed = Inline_static_exit_cross_module_lib.pick(id(0), id(5), id(false))
let computedOther = Inline_static_exit_cross_module_lib.pick(id(1), id(5), id(true))

// Parameters as arguments: the call is inlined while compiling to JavaScript.
let both = (x, y, flag) =>
  Inline_static_exit_cross_module_lib.pick(x, y, true) ++
  " " ++
  Inline_static_exit_cross_module_lib.pick(x, y, flag)

module First = Inline_static_exit_cross_module_lib.Make({
  let v = 0
  let w = 5
  let flag = true
})

module Second = Inline_static_exit_cross_module_lib.Make({
  let v = id(0)
  let w = id(5)
  let flag = id(false)
})

describe(__MODULE__, () => {
  test("two inlined copies", () => {
    eq(__LOC__, constant, "t")
    eq(__LOC__, computed, "a05")
    eq(__LOC__, computedOther, "other")
  })

  test("two inlined copies in a function", () => {
    eq(__LOC__, both(0, 5, false), "t a05")
    eq(__LOC__, both(1, 5, false), "other other")
  })

  test("functor applied twice", () => {
    eq(__LOC__, First.r, "t")
    eq(__LOC__, Second.r, "a05")
  })
})
