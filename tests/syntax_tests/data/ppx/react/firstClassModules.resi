@@jsxConfig({version: 4, mode: "classic"})

module Select: {
  module type T = {
    type key
    type t
  }

  @react.component
  let make: (
    ~model: module(T with type t = 'a and type key = 'key),
    ~selected: option<'key>,
    ~onChange: option<'key> => unit,
    ~items: array<'a>,
  ) => React.element
}
