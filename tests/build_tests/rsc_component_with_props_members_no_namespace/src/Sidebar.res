module Provider = {
  type props = {children: React.element}

  @react.componentWithProps
  let make = props => props.children
}
