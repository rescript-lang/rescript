module StrSet = AppliedFunctor.MakeSet({
  type t = string
  let compare = (a: string, b) => a < b ? -1 : a > b ? 1 : 0
})

let s = StrSet.add([], "a")
//             ^def

let t: StrSet.t = []
//            ^hov

// let _ = StrSet.add(
//                    ^she

let value: StrSet.t = []
//   ^typ

let s2 = StrSet.add([], "b")
//              ^ref

module N = AppliedFunctor.WithNested(AppliedFunctor.IntOrd)

let nv = N.Nested.value
//         ^def

let nv2 = N.Nested.value
//                 ^def

let nv3 = N.Nested.value
//                 ^ref

module Shapes = AppliedFunctor.WithShapes(AppliedFunctor.IntOrd)

let p: Shapes.point = {x: 1}
//  ^typ

let px = p.x
//         ^def

let box = Shapes.Box(1)
//               ^def

module Local = (O: AppliedFunctor.Ord) => {
  module Inner = {
    type t = O.t
  }
}

module LocalApplied = Local(AppliedFunctor.IntOrd)

let li: LocalApplied.Inner.t = 1
//  ^typ

let li2: LocalApplied.Inner.t = 2
//                   ^def

let nv4 = N.Nested.value
//          ^ref

let p2: Shapes.point = {x: 2}
//             ^ref
