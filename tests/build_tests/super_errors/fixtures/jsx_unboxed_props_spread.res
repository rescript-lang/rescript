module React = {
  type element = Jsx.element
  type component<'props> = Jsx.component<'props>

  external component: Jsx.componentLike<'props, element> => component<'props> =
    "%component_identity"

  external string: string => element = "%identity"

  @module("react/jsx-runtime")
  external jsx: (component<'props>, 'props) => element = "jsx"
}

@unboxed type props = {title: string}

module Title = {
  @react.componentWithProps
  let make = (props: props) => React.string(props.title)
}

let props = {title: "x"}
let el = <Title {...props} />
