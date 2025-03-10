@@jsxConfig({version: 4})

module V4C = {
  @module("c") @react.component
    external make: (
      ~x: t<'a>,
      ~children: React.element,
    ) => React.element = "component"
}
