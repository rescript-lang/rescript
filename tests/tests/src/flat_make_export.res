let make = () => "top"

module One = {
  let make = () => "one"

  let keep = () => "one-keep"
}

module Two = {
  let keep = () => "two-keep"

  module Inner = {
    let make = () => "two"

    let keep = () => "two-inner-keep"
  }
}

module Three = {
  module Inner = {
    let keep = () => "three-inner-keep"

    module Deep = {
      let make = () => "three"

      let keep = () => "three-deep-keep"
    }
  }
}

module Ambiguous = {
  module B = {
    let make = () => "nested"
  }

  let \"B$make" = () => "exotic-value"
}

module rec RecursiveA: {
  let make: unit => string
} = {
  let make = () => RecursiveB.value()
}
and RecursiveB: {
  let value: unit => string
} = {
  let value = () => "recursive"
}

module Coerced: {
  let make: unit => string
} = {
  let hidden = () => "coerced"

  let make = () => hidden()
}

module Rebound = {
  let make = () => "rebound"
  let make = Some(make)
}

module Make = (
  X: {
    let value: string
  },
) => {
  let make = () => X.value
}

module Applied = Make({
  let value = "applied"
})

module Included = {
  include One
}

module Aliased = One
