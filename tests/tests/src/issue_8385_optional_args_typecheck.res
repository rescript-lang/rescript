@@config({flags: ["-bs-jsx", "4"]})

module type Test = {
  @react.component
  let make: (~test: int=?) => React.element
}

module A: Test = {
  @react.component
  let make = (~test: option<int>=42) => {
    let _: int = test
    React.null
  }
}
