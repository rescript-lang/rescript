// The doc comments of a functor of another file.
module StrSet = AppliedFunctorDocs.MakeSet({
  type t = string
})

let s = StrSet.add([], "a")
//              ^hov

// StrSet.a
//         ^com
