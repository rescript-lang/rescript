open Mocha
open Test_utils

// The type checker memoizes type path lookups per request. Each section
// resolves the same type path text against environments that bind it
// differently, so a stale lookup would make it fail to type check or change
// the generated code.

// A type shadowed in a nested module, then used again outside it.
type t = int

module Inner = {
  let before: t = 1
  type t = string
  let after: t = "two"
}

let first: t = Inner.before
let second = Inner.after

// A local open shadows a type, and the outer type is used again afterwards.
module Rec = {
  type t = {a: int}
}

type r = {b: string}

let fromOpen = () => {
  open Rec
  let v: t = {a: 3}
  v.a
}

let afterOpen = () => {
  let v: r = {b: "after"}
  v.b
}

// A local module shadows a top-level module of the same name.
module Shadow = {
  type t = int
  let v: t = 10
}

let shadowed = () => {
  module Shadow = {
    type t = string
    let v: t = "shadowed"
  }
  Shadow.v ++ "!"
}

let unshadowed = Shadow.v + 1

// Module aliases: the alias path normalizes to the aliased module's type.
module Color = {
  type t = Red | Green
}

module Colour = Color

let colourName = (c: Colour.t) =>
  switch c {
  | Red => "red"
  | Green => "green"
  }

let colorAsColour = (c: Color.t): Colour.t => c

// A module type constraint makes a type abstract outside the module.
module Counter: {
  type t
  let make: int => t
  let get: t => int
} = {
  type t = int
  let make = n => n * 2
  let get = n => n
}

// Functor applications give the same type path different meanings.
module type Value = {
  type t
  let value: t
  let show: t => string
}

module Wrap = (V: Value) => {
  type t = V.t
  let wrapped: t = V.value
  let describe = () => "<" ++ V.show(wrapped) ++ ">"
}

module IntWrap = Wrap({
  type t = int
  let value = 4
  let show = n => Int.toString(n)
})

module StringWrap = Wrap({
  type t = string
  let value = "s"
  let show = s => s
})

let wrappedSum = IntWrap.wrapped + 1
let wrappedString = StringWrap.wrapped ++ "!"

// First-class modules and locally abstract types.
let showPacked = (type a, m: module(Value with type t = a)) => {
  module M = unpack(m)
  M.show(M.value)
}

let packedInt = showPacked(
  module(
    {
      type t = int
      let value = 7
      let show = n => Int.toString(n)
    }
  ),
)

let pair = (type a, x: a): array<a> => [x, x]

// A recursive type group is checked with placeholder declarations first,
// then with the real ones under the same identifiers.
type rec node<'a> = Leaf('a) | Branch(branches<'a>)
and branches<'a> = list<node<'a>>

let tree = Branch(list{Leaf(1), Branch(list{})})

let branchCount = (n: node<int>) =>
  switch n {
  | Leaf(_) => 0
  | Branch(b) => List.length(b)
  }

// Recursive modules refer to each other's types, and are typed against an
// approximation of their signatures first.
module rec Tree: {
  type t = Leaf | Node(Forest.t)
  let size: t => int
  let width: t => int
} = {
  type t = Leaf | Node(Forest.t)

  let width = t =>
    switch t {
    | Leaf => 0
    | Node(f) => List.length(f)
    }

  let size = t =>
    switch t {
    | Leaf => 1
    | Node(f) => 1 + Forest.size(f)
    }
}
and Forest: {
  type t = list<Tree.t>
  let size: t => int
} = {
  type t = list<Tree.t>
  let size = f => List.reduce(f, 0, (acc, t) => acc + Tree.size(t))
}

describe(__MODULE__, () => {
  test("shadowed and opened types", () => {
    eq(__LOC__, (1, "two"), (first, second))
    eq(__LOC__, (3, "after"), (fromOpen(), afterOpen()))
    eq(__LOC__, ("shadowed!", 11), (shadowed(), unshadowed))
  })
  test("aliases and constraints", () => {
    eq(__LOC__, ("red", "green"), (colourName(Red), colourName(colorAsColour(Green))))
    eq(__LOC__, 6, Counter.get(Counter.make(3)))
  })
  test("functors, first-class and recursive modules", () => {
    eq(__LOC__, (5, "s!"), (wrappedSum, wrappedString))
    eq(__LOC__, ("<4>", "<s>"), (IntWrap.describe(), StringWrap.describe()))
    eq(__LOC__, ("7", [1, 1]), (packedInt, pair(1)))
    eq(__LOC__, 3, Tree.size(Node(list{Leaf, Node(list{})})))
    eq(__LOC__, (2, 2), (branchCount(tree), Tree.width(Node(list{Leaf, Leaf}))))
  })
})
