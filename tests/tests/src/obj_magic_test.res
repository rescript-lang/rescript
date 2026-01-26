@@config({
  flags: [
    /* "-drawlambda" */
  ],
})

/* let empty_backtrace  = Obj.obj (Obj.new_block Obj.abstract_tag 0) */

let is_block = x => Js.typeof(Obj.repr(x)) != "number"

open Mocha
open Test_utils

describe(__MODULE__, () => {
  test("is_block_test1", () => {
    eq(__LOC__, false, is_block(3))
  })
  test("is_block_test2", () => {
    eq(__LOC__, true, is_block(list{3}))
  })
  test("is_block_test3", () => {
    eq(__LOC__, true, is_block("x"))
  })
  test("is_block_test4", () => {
    eq(__LOC__, false, is_block(3.0))
  })
})
