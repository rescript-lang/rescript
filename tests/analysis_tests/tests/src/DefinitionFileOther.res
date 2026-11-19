// The same declarations reached from another file.

let a = DefinitionFile.Aliased.add(1)
//                             ^def

let t: DefinitionFile.Aliased.t = []
//  ^typ

let f: DefinitionFile.IncludesOther.base = 1
//  ^hov

module Again = DefinitionFile.Make(DefinitionFileSource.IntOrd)
let b = Again.add(1)
//            ^def

// Through aliases in two other files.
module Bridged = DefinitionFile.MakeOther(DefinitionFileSource.IntOrd)
//      ^hov
let c = Bridged.add(1)
//              ^def
