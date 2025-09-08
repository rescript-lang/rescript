module A: {
  @react.component
  let make: (~x: string) => React.element
}

module NoProps: {
  @react.component
  let make: unit => React.element
}