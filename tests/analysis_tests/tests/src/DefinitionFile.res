// Declarations reached through a functor alias or an include are in
// DefinitionFileSource.res, not in this file.

module Make = DefinitionFileSource.MakeSet
module Aliased = Make(DefinitionFileSource.IntOrd)
let a = Aliased.add(1)
//              ^def

let t: Aliased.t = []
//  ^typ

module Make2 = Make
module Chain = Make2(DefinitionFileSource.IntOrd)
let b = Chain.add(1)
//            ^def

module Nested = {
  module Make = DefinitionFileSource.MakeSet
}
module FromNested = Nested.Make(DefinitionFileSource.IntOrd)
let c = FromNested.add(1)
//                 ^def

module FromOtherAlias = DefinitionFileSource.AliasSet(DefinitionFileSource.IntOrd)
let d = FromOtherAlias.add(1)
//                     ^def

let sign = (x: Aliased.t) => x
let e = sign([])
//           ^she

module IncludesOther = {
  include DefinitionFileSource.Base
}
let f: IncludesOther.base = 1
//  ^hov

module LocalBase = DefinitionFileSource.Base
module IncludesLocalAlias = {
  include LocalBase
}
let g: IncludesLocalAlias.base = 1
//  ^hov

// Declarations in this file stay in this file.

module Own = {
  type own = int
}
module IncludesOwn = {
  include Own
}
let h: IncludesOwn.own = 1
//  ^hov

module LocalMake = (O: DefinitionFileSource.Ord) => {
  let local = 1
}
module LocalAlias = LocalMake
module FromLocal = LocalAlias(DefinitionFileSource.IntOrd)
let i = FromLocal.local
//                ^def

let j = 1
let k = j
//      ^def

// An alias of DefinitionFileSource.AliasSet, itself an alias there.
module MakeOther = DefinitionFileSource.AliasSet
