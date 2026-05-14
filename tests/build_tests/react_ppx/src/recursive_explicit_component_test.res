module SelfCreateElement = {
  @react.component
  let rec make = (~foo) => React.createElement(React.component(make), {foo: foo - 1})
}

module RawSiblingCreateElement = {
  @react.component
  let rec make = (~foo) => React.createElement(React.component(other), {foo: foo})
  and other = ({foo}) => React.string(foo->Int.toString)
}

module ComponentWithProps = {
  type props = {foo: int}

  @react.componentWithProps
  let rec make = (props: props) =>
    if props.foo <= 0 {
      React.null
    } else {
      React.createElement(React.component(make), {foo: props.foo - 1})
    }
}

let componentWithPropsElement = React.createElement(ComponentWithProps.make, {foo: 1})
