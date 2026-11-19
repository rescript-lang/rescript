module type MT = {
  let inMt: int
  //  ^ren renamed
}
module Impl: MT = {
  let inMt = 2
}
let a = Impl.inMt

module type WithSig = {
  module type S = {
    let sigValue: int
  }
  module Nested: S
}
module Sealed: WithSig = {
  module type S = {
    let sigValue: int
  }
  module Nested = {
    let sigValue = 1
  }
}
let sv = Sealed.Nested.sigValue
//                     ^ref

module M: {
  module Inner: {
    let v: int
    //  ^ref
  }
} = {
  module Inner = {
    let v = 1
  }
}

module type HasInner = {
  type t
  module Inner: {
    let w: int
  }
}
module W: HasInner with type t = int = {
  type t = int
  module Inner = {
    let w = 1
  }
}
