open Mocha
open Test_utils
open Belt

module N = List

module V = Ext_pervasives_test.LargeFile

module J = Js.Json

module type X = module type of List

let f = x => {
  module L = List
  Js.log(x)
  Js.log(List.length(x))
  module(L: X)
}

let a = {
  let h = f(list{})
  let module(L: X) = h
  L.length(list{1, 2, 3})
}

eq(__LOC__, a, 3)
describe(__MODULE__, () => {
  test("module_alias_test", () => {
    eq(__LOC__, a, 3)
  })
})
