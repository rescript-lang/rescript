// SPDX-License-Identifier: (LGPL-3.0-or-later AND MIT)
// End-to-end test: Module system
// Tests module definitions, signatures, functors, first-class modules,
// include, open, and recursive modules.

let eq = (a, b) => a == b

// ─── Simple module definitions ──────────────────────────────────────
module Simple = {
  let x = 1
  let y = 2
  let add = (a, b) => a + b
}
Test.run(__POS_OF__("mod: simple x"), Simple.x, eq, 1)
Test.run(__POS_OF__("mod: simple add"), Simple.add(3, 4), eq, 7)

// ─── Nested modules ────────────────────────────────────────────────
module Level1 = {
  let a = 10
  module Level2 = {
    let b = 20
    module Level3 = {
      let c = 30
      let sum = () => a + b + c
    }
  }
}
Test.run(__POS_OF__("mod: nested L1"), Level1.a, eq, 10)
Test.run(__POS_OF__("mod: nested L2"), Level1.Level2.b, eq, 20)
Test.run(__POS_OF__("mod: nested L3"), Level1.Level2.Level3.c, eq, 30)
Test.run(__POS_OF__("mod: nested sum"), Level1.Level2.Level3.sum(), eq, 60)

// ─── Module aliases ─────────────────────────────────────────────────
module L3 = Level1.Level2.Level3
Test.run(__POS_OF__("mod: alias"), L3.c, eq, 30)
Test.run(__POS_OF__("mod: alias fn"), L3.sum(), eq, 60)

// ─── Module type constraints ────────────────────────────────────────
module type Printable = {
  type t
  let toString: t => string
  let fromInt: int => t
}

module PrintableInt: Printable = {
  type t = int
  let toString = Int.toString
  let fromInt = x => x
}

module PrintableString: Printable = {
  type t = string
  let toString = x => x
  let fromInt = Int.toString
}

Test.run(
  __POS_OF__("mod: type constraint int"),
  PrintableInt.toString(PrintableInt.fromInt(42)),
  eq,
  "42",
)
Test.run(
  __POS_OF__("mod: type constraint str"),
  PrintableString.toString(PrintableString.fromInt(42)),
  eq,
  "42",
)

// ─── Module functors ────────────────────────────────────────────────
module MakeStack = (Item: {type t}) => {
  type t = array<Item.t>
  let make = (): t => []
  let push = (stack: t, item: Item.t): t => Array.concat([item], stack)
  let pop = (stack: t) =>
    switch stack {
    | [top, ...rest] => Some((top, rest))
    | [] => None
    }
  let peek = (stack: t) =>
    switch stack {
    | [top, ..._] => Some(top)
    | [] => None
    }
  let size = (stack: t) => Array.length(stack)
  let isEmpty = (stack: t) => Array.length(stack) == 0
}

module IntStack = MakeStack({type t = int})
module StrStack = MakeStack({type t = string})

let stack = IntStack.make()->IntStack.push(1)->IntStack.push(2)->IntStack.push(3)
Test.run(__POS_OF__("mod: functor size"), IntStack.size(stack), eq, 3)
Test.run(__POS_OF__("mod: functor peek"), IntStack.peek(stack), eq, Some(3))
let (top, rest) = IntStack.pop(stack)->Option.getExn
Test.run(__POS_OF__("mod: functor pop"), top, eq, 3)
Test.run(__POS_OF__("mod: functor rest"), IntStack.size(rest), eq, 2)

let strStack = StrStack.make()->StrStack.push("hello")->StrStack.push("world")
Test.run(__POS_OF__("mod: functor str"), StrStack.peek(strStack), eq, Some("world"))

// ─── Module includes ────────────────────────────────────────────────
module Base = {
  let x = 1
  let y = 2
  let sum = () => x + y
}

module Extended = {
  include Base
  let z = 3
  let fullSum = () => sum() + z
}

Test.run(__POS_OF__("mod: include x"), Extended.x, eq, 1)
Test.run(__POS_OF__("mod: include y"), Extended.y, eq, 2)
Test.run(__POS_OF__("mod: include sum"), Extended.sum(), eq, 3)
Test.run(__POS_OF__("mod: include new"), Extended.z, eq, 3)
Test.run(__POS_OF__("mod: include fullSum"), Extended.fullSum(), eq, 6)

// ─── Module open ────────────────────────────────────────────────────
let openResult = {
  open Base
  sum() * 10
}
Test.run(__POS_OF__("mod: open scope"), openResult, eq, 30)

// Open in function
let withOpen = () => {
  open Extended
  fullSum()
}
Test.run(__POS_OF__("mod: open in fn"), withOpen(), eq, 6)

// ─── First-class modules ───────────────────────────────────────────
module type Serializable = {
  type t
  let encode: t => string
  let decode: string => option<t>
}

module IntSerializer: Serializable with type t = int = {
  type t = int
  let encode = Int.toString
  let decode = Int.fromString
}

module FloatSerializer: Serializable with type t = float = {
  type t = float
  let encode = Float.toString
  let decode = Float.fromString
}

let roundtrip = (type a, module(S: Serializable with type t = a), v: a): option<a> => {
  let encoded = S.encode(v)
  S.decode(encoded)
}

Test.run(
  __POS_OF__("mod: first-class int"),
  roundtrip(module(IntSerializer), 42),
  eq,
  Some(42),
)
Test.run(
  __POS_OF__("mod: first-class float"),
  roundtrip(module(FloatSerializer), 3.14),
  eq,
  Some(3.14),
)

// ─── Module with complex types ──────────────────────────────────────
module Collection = {
  type t<'a> = {items: array<'a>, count: int}

  let empty = (): t<'a> => {items: [], count: 0}
  let add = (c: t<'a>, item: 'a): t<'a> => {
    items: Array.concat(c.items, [item]),
    count: c.count + 1,
  }
  let toArray = (c: t<'a>) => c.items
  let size = (c: t<'a>) => c.count
  let map = (c: t<'a>, f: 'a => 'b): t<'b> => {
    items: Array.map(c.items, f),
    count: c.count,
  }
}

let col = Collection.empty()->Collection.add(1)->Collection.add(2)->Collection.add(3)
Test.run(__POS_OF__("mod: collection size"), Collection.size(col), eq, 3)
Test.run(__POS_OF__("mod: collection toArray"), Collection.toArray(col), eq, [1, 2, 3])

let doubled = Collection.map(col, x => x * 2)
Test.run(__POS_OF__("mod: collection map"), Collection.toArray(doubled), eq, [2, 4, 6])

// ─── Module type of ────────────────────────────────────────────────
module type BaseType = module type of Base
module BaseCopy: BaseType = {
  let x = 10
  let y = 20
  let sum = () => x + y
}
Test.run(__POS_OF__("mod: type of"), BaseCopy.sum(), eq, 30)

// ─── Abstract module types ─────────────────────────────────────────
module type Container = {
  type t<'a>
  let empty: unit => t<'a>
  let add: (t<'a>, 'a) => t<'a>
  let toArray: t<'a> => array<'a>
}

module ArrayContainer: Container = {
  type t<'a> = array<'a>
  let empty = () => []
  let add = (c, item) => Array.concat(c, [item])
  let toArray = c => c
}

let ac = ArrayContainer.empty()->ArrayContainer.add(1)->ArrayContainer.add(2)
Test.run(__POS_OF__("mod: abstract container"), ArrayContainer.toArray(ac), eq, [1, 2])

// ─── Shadowing in modules ──────────────────────────────────────────
module Shadow = {
  let x = 1
  module Inner = {
    let x = 2 // shadows outer x
    let getX = () => x
  }
  let getX = () => x
}
Test.run(__POS_OF__("mod: shadow outer"), Shadow.getX(), eq, 1)
Test.run(__POS_OF__("mod: shadow inner"), Shadow.Inner.getX(), eq, 2)
