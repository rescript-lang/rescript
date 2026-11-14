// Uses of compilation units whose signatures the compiler only copies when
// they are read: module aliases and chains of them, values reached through an
// alias, functors and module types of dependencies, nested and recursive
// modules, and includes.

module A = Stdlib.Array
module B = A

module C = {
  module D = B
}

module L = Belt.List

let mapped = C.D.map([1, 2, 3], x => x + 1)
let viaBelt = Belt.Array.map([1, 2], x => x * 10)
let viaList = L.toArray(L.map(list{1, 2}, x => x - 1))

module IntCmp = Belt.Id.MakeComparable({
  type t = int
  let cmp = (a: int, b: int) => compare(a, b)
})

let set = Belt.Set.fromArray([3, 1, 2], ~id=module(IntCmp))

module type ArrayLike = module type of Belt.Array

module MyArray: ArrayLike = Belt.Array

module Extended = {
  include Belt.Option
  let orZero = x => getWithDefault(x, 0)
}

module rec Tree: {
  type t = Leaf | Node(Belt.Array.t<Tree.t>)
  let size: t => int
} = {
  type t = Leaf | Node(Belt.Array.t<Tree.t>)

  let size = t =>
    switch t {
    | Leaf => 1
    | Node(children) => Belt.Array.reduce(children, 1, (acc, c) => acc + Tree.size(c))
    }
}

open Mocha
open Test_utils

describe(__MODULE__, () => {
  test("alias chains", () => {
    eq(__LOC__, [2, 3, 4], mapped)
    eq(__LOC__, [10, 20], viaBelt)
    eq(__LOC__, [0, 1], viaList)
  })
  test("functors and module types of dependencies", () => {
    eq(__LOC__, [1, 2, 3], Belt.Set.toArray(set))
    eq(__LOC__, 2, MyArray.length([1, 2]))
  })
  test("include and recursive modules", () => {
    eq(__LOC__, 0, Extended.orZero(None))
    eq(__LOC__, 3, Tree.size(Node([Leaf, Leaf])))
  })
})
