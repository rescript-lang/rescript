module React = {
  type element = Jsx.element
  type componentLike<'props, 'return> = Jsx.componentLike<'props, 'return>
  type component<'props> = Jsx.component<'props>

  external component: componentLike<'props, element> => component<'props> = "%component_identity"
  external string: string => element = "%identity"
  @module("react/jsx-runtime")
  external jsx: (component<'props>, 'props) => element = "jsx"

  @module("react/jsx-runtime")
  external jsxs: (component<'props>, 'props) => element = "jsxs"
}

module ReactDOM = {
  external someElement: React.element => option<React.element> = "%identity"
  @module("react/jsx-runtime")
  external jsx: (string, JsxDOM.domProps) => Jsx.element = "jsx"
}

module Wrapper = {
  type props = {value: string}
  let make = (props: props) => <div> {React.string(props.value)} </div>
}

let _ = <Wrapper value="hello" />
