@@jsxConfig({version: 4, mode: "classic"})

module Select = {
  module type T = {
    type key
    type t
  }

  @react.component
  let make = (
    type a key,
    ~model as module(T: T with type t = a and type key = key),
    ~selected: option<key>,
    ~onChange: option<key> => unit,
    ~items: array<a>,
  ) => {
    let _ = (model, selected, onChange, items)
    <div />
  }
}

module C6 = {
  module type Comp = {
    let xx: int
    @react.component
    let make: unit => React.element
  }

  @react.component
  let make = (~comp as module(Comp: Comp), ~x as (a, b)) => Comp.xx
}

module External = {
  module type T = {
    type key
    type t
  }

  @react.component @module("c")
  external make: (
    ~model: module(T with type t = 'a and type key = 'key),
    ~selected: option<'key>,
    ~onChange: option<'key> => unit,
    ~items: array<'a>,
  ) => React.element = "default"
}
