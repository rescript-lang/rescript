module C = {
  @react.component @react.componentWithProps
  let make = (~label) => React.string(label)
}
