@@config({flags: ["-bs-cross-module-opt"]})

// Inlining another module's function renames its local variables. An
// assignment to one used to keep the old name, a variable this module doesn't
// declare.
open Mocha
open Test_utils

@inline(never) let id = x => x

let addBumped = (x, y) =>
  Inline_assign_cross_module_lib.bumpTwice(x) + Inline_assign_cross_module_lib.bumpTwice(y)

let bumpConstants = () => (
  Inline_assign_cross_module_lib.bumpTwice(5),
  Inline_assign_cross_module_lib.bumpTwice(3),
)

describe(__MODULE__, () => {
  test("a variable the inlined function assigns, twice in a function", () => {
    eq(__LOC__, 14, addBumped(id(3), id(4)))
  })

  test("a variable the inlined function assigns, with a constant argument", () => {
    eq(__LOC__, (12, 4), bumpConstants())
  })
})
