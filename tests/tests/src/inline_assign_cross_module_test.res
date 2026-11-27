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

let readBeforeAssign = () => Inline_assign_cross_module_lib.readThenAssign(5, p => () => p)

let copyBeforeAssign = () => Inline_assign_cross_module_lib.copyThenAssign(1)

let readBeforeAssignBound = () => {
  let capture = p => () => p
  Inline_assign_cross_module_lib.readThenAssign(5, capture)
}

describe(__MODULE__, () => {
  test("a variable the inlined function assigns, twice in a function", () => {
    eq(__LOC__, 14, addBumped(id(3), id(4)))
  })

  test("a variable the inlined function assigns, with a constant argument", () => {
    eq(__LOC__, (12, 4), bumpConstants())
  })

  test("a read of an assigned variable, passed to a closure before the assignment", () => {
    eq(__LOC__, 15, readBeforeAssign())
  })

  test("a copy of an assigned variable, made before the assignment", () => {
    eq(__LOC__, 3, copyBeforeAssign())
  })

  test("a read of an assigned variable, passed to a bound closure before the assignment", () => {
    eq(__LOC__, 15, readBeforeAssignBound())
  })
})
