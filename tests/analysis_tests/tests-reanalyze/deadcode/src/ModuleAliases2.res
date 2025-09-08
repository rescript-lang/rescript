@genType
type record = {
  x: int,
  y: string,
}

module Outer = {
  @genType
  type outer = {outer: string}

  module Inner = {
    @genType
    type inner = {inner: string}
  }
}

module OuterAlias = Outer

module InnerAlias = OuterAlias.Inner

let q = 42

