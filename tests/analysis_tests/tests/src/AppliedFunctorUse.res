module StrSet = AppliedFunctor.MakeSet({
  type t = string
  let compare = (a: string, b) => a < b ? -1 : a > b ? 1 : 0
})

let s = StrSet.add([], "a")
//             ^def

let t: StrSet.t = []
//            ^hov
