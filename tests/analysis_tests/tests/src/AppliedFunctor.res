module type Ord = {
  type t
  let compare: (t, t) => int
}

module MakeSet = (O: Ord) => {
  type t = array<O.t>
  let add = (s: t, x: O.t) => s->Array.concat([x])
  //   ^ref
}

module IntSet = MakeSet({
  type t = int
  let compare = (a: int, b) => a - b
})

let s = IntSet.add([], 1)
//             ^hov

let s3 = IntSet.add([], 3)
//              ^ref

let s4 = IntSet.add([], 4)
//              ^ren insert

let t: IntSet.t = []
//            ^hov

// let s2 = IntSet.
//                 ^com

module WithNested = (O: Ord) => {
  module Nested = {
    //     ^ref
    let value = 1
  }
}

module IntOrd = {
  type t = int
  let compare = (a: int, b) => a - b
}

module N = WithNested(IntOrd)

let n = N.Nested.value
//        ^ref

module WithShapes = (O: Ord) => {
  type point = {x: int}
  type shape = Dot | Box(int)
}

module Shapes = WithShapes(IntOrd)

let p: Shapes.point = {x: 1}
let px = p.x
//         ^def

let shape = Shapes.Box(1)
//                 ^def

let py = p.x
//         ^ren y
