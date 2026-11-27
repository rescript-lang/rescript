@@config({flags: ["-bs-cross-module-opt"]})

// A call to another module's function that takes a parameter of the caller is
// inlined while compiling to JavaScript, after a local ref has become a
// variable. An argument that reads the variable used to be substituted into
// the body, past the argument that assigns it.
open Mocha
open Test_utils

let readThenBump = x => {
  let s = ref(0)
  let bumpS = () => {
    s := s.contents + 10
    1
  }
  Inline_arg_order_cross_module_lib.minus3(x, s.contents, bumpS())
}

let bumpThenRead = x => {
  let s = ref(0)
  let bumpS = () => {
    s := s.contents + 10
    1
  }
  Inline_arg_order_cross_module_lib.minus3(x, bumpS(), s.contents)
}

let readThenCall = () => {
  let s = ref(0)
  Inline_arg_order_cross_module_lib.callNext(() => {
    s := s.contents + 1
    y => y
  }, s.contents)
}

describe(__MODULE__, () => {
  test("a read of a local ref before an argument that assigns it", () => {
    eq(__LOC__, 1, readThenBump(0))
  })

  test("a read of a local ref after an argument that assigns it", () => {
    eq(__LOC__, 9, bumpThenRead(0))
  })

  test("a read of a local ref before a callee that assigns it", () => {
    eq(__LOC__, 0, readThenCall())
  })
})
