type props = {label: string}

module C = {
  @react.componentWithProps(: props)
  let make = (props: props) => React.string(props.label)
}
