module React = {
  type element = unit
  let null = ()
}

module Foo: {
  @react.component let make: (~htmlAttributes: array<float>=?, unit) => React.element
} = {
  @react.component let make = (~htmlAttributes as _=?, ()) => React.null
}
