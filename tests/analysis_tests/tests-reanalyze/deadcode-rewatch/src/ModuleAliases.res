module Outer = {
  module Inner = {
    type innerT = {inner: string}
  }
}

module Outer2 = {
  module OuterInnerAlias = Outer.Inner
  module Inner2 = {
    module InnerNested = {
      type t = {nested: int}
    }
    module OuterInnerAlias2 = OuterInnerAlias
  }
}

module Outer2Alias = Outer2

module InnerNestedAlias = Outer2.Inner2.InnerNested

@genType
let testNested = (x: InnerNestedAlias.t) => x

@genType
let testInner = (x: Outer2Alias.OuterInnerAlias.innerT) => x

@genType
let testInner2 = (x: Outer2Alias.Inner2.OuterInnerAlias2.innerT) => x

