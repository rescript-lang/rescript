module A = {
  @react.component
  let make = (~x) => React.string(x)
}

module NoProps = {
  @react.component
  let make = () => <div />
}