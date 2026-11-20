module S = PathRefs.MakeSet(PathRefs.IntOrd)
let s = S.add(1)
module Make = PathRefs.MakeSet
module Aliased = Make(PathRefs.IntOrd)
let a = Aliased.add(1)
module Fin = PathRefs.First(PathRefs.IntOrd)
let p = Fin.pair
