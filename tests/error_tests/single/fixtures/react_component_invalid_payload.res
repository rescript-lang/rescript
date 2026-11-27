module C = {
  @react.component("Button")
  let make = (~label) => React.string(label)
}
