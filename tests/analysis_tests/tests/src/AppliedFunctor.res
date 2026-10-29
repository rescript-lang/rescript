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

module WithModType = (O: Ord) => {
  module type S = {
    let value: int
    //  ^ref
  }
  module Nested: S = {
    let value = 1
  }
}

module Applied = WithModType(IntOrd)

let nv = Applied.Nested.value
//                      ^hov

// let nv2 = Applied.Nested.
//                          ^com

let nv3 = Applied.Nested.value
//                       ^ren renamed

module WithDeepModType = (O: Ord) => {
  module Outer = {
    module type T = {
      module Inner: {
        let deep: int
        //  ^ref
      }
      module type Abstract
    }
    module First: T = {
      module Inner = {
        let deep = 1
      }
      module type Abstract = {}
    }
    module Second: T = {
      module Inner = {
        let deep = 2
      }
      module type Abstract = {}
    }
  }
}

module Deep = WithDeepModType(IntOrd)

let d1 = Deep.Outer.First.Inner.deep
let d2 = Deep.Outer.Second.Inner.deep
//                               ^ref

module type WithSig = {
  module type S = {
    let sigValue: int
    //  ^ref
  }
  module Nested: S
}

module WithSigResult = (O: Ord): WithSig => {
  module type S = {
    let sigValue: int
  }
  module Nested = {
    let sigValue = 1
  }
}

module AppliedSig = WithSigResult(IntOrd)

let sv = AppliedSig.Nested.sigValue
//                         ^ref

module UseAppliedModType = (X: Applied.S) => {
  let v = X.value
  //        ^ref
}

module UseDeepModType = (Y: Deep.Outer.T) => {
  let d = Y.Inner.deep
  //              ^ref
}
