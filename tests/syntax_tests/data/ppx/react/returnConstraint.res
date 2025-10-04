@@jsxConfig({version: 4})

module Standard = {
  @react.component
  let make = () => React.string("ok")
}

module ForwardRef = {
  @react.component
  let make = React.forwardRef((_, _ref) => ReactDOM.jsx("div", {}))
}

module WithProps = {
  type props = {value: int}

  @react.componentWithProps
  let make = (props: props) =>
    ReactDOM.jsx("span", {children: ?ReactDOM.someElement({React.int(props.value)})})
}

module Async = {
  @react.component
  let make = async () => ReactDOM.jsx("div", {children: ?ReactDOM.someElement({React.string("async")})})
}
