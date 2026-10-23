module Provider: {
  @react.component
  let make: (~children: React.element) => React.element
}

module Menu: {
  module Item: {
    @react.component
    let make: (~children: React.element) => React.element
  }
}
