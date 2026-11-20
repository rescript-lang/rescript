// A module type reached through an include is declared where it comes from.
module Base = {
  module type MT = {
    type inner
    //   ^ren renamedInner
    let inMt: int
    //  ^ref
  }
  module Impl: MT = {
    type inner = int
    let inMt = 2
  }
}

module Inc = {
  include Base
}

let a = Inc.Impl.inMt
//               ^ref
