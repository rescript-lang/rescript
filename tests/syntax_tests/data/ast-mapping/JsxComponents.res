module A = {
  @react.component
  let make = (~x) => React.string(x)
}

module B = {
  /** Doc */
  @deprecated @jsx.component(: sharedProps<string>) @genType
  let make = (~x) => React.string(x)
}

module C = {
  @react.componentWithProps
  let make = (props: props) => React.string(props.x)
}

module D = {
  @jsx.componentWithProps
  let make = (props: props) => React.string(props.x)
}

module E = {
  @react.component @module("x")
  external make: (~x: string) => React.element = "default"
}

module F: {
  @react.component
  let make: (~x: string) => React.element
  @jsx.componentWithProps
  let other: props => React.element
} = {
  @react.component
  let make = (~x) => React.string(x)
  let other = props => React.string(props.x)
}

let f = () => {
  @react.component
  let make = (~x) => React.string(x)
  make
}
