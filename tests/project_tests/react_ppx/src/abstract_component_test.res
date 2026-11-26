module PolyVariantLowerBound = {
  @react.component
  let make = (~x) =>
    switch x {
    | #a => React.string("A")
    | #b => React.string("B")
    | _ => React.string("other")
    }
}

module GenericRenderProp = {
  @react.component
  let make = (~x: 'x, ~render: 'x => React.element) => render(x)
}
