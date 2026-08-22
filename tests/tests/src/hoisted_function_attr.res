module One = {
  @res.hoistedFunction
  let make = () => "one"

  let keep = () => "one-keep"
}

module Two = {
  let keep = () => "two-keep"

  module Inner = {
    @res.hoistedFunction
    let make = () => "two"

    let keep = () => "two-inner-keep"
  }
}

module Three = {
  module Inner = {
    let keep = () => "three-inner-keep"

    module Deep = {
      @res.hoistedFunction
      let make = () => "three"

      let keep = () => "three-deep-keep"
    }
  }
}

module Escaped = {
  @res.hoistedFunction
  let \"switch" = () => "keyword"

  @res.hoistedFunction
  let \"$plus" = () => "dollar"
}

module Operator = {
  @res.hoistedFunction
  let \"+" = () => "operator"
}

module Ambiguous = {
  module B = {
    @res.hoistedFunction
    let make = () => "nested"
  }

  let \"B$make" = () => "exotic"
}

module rec RecursiveA: {
  let make: unit => string
} = {
  @res.hoistedFunction
  let make = () => RecursiveB.value()
}
and RecursiveB: {
  let value: unit => string
} = {
  let value = () => "recursive"
}

module Typed = {
  @res.hoistedFunction
  let make: unit => string = () => "typed"
}
