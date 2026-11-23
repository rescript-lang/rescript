// Uses of modules that AppliedFunctorUse creates by applying functors of
// AppliedFunctor
let a = AppliedFunctorUse.StrSet.add([], "a")
//                               ^def

let a2 = AppliedFunctorUse.StrSet.add([], "b")
//                                ^ref

let nv = AppliedFunctorUse.N.Nested.value
//                                  ^ref

let ps = AppliedFunctorUse.FinalPair.second
//                                   ^ref

let cv = AppliedFunctorCollide.v0 + AppliedFunctorCollide.v1

// A functor reached through an alias in a third file
module R = AppliedFunctorAlias.Re(AppliedFunctor.IntOrd)
let r = R.add([], 1)
let r2 = R.add([], 2)
//         ^ref

// A partial application in the functor's file, completed here
module Fin = AppliedFunctor.FirstPair(AppliedFunctor.IntOrd)
let f = Fin.second
let f2 = Fin.second
//           ^ref
