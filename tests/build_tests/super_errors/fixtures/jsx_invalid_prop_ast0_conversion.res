@@config({flags: ["-bs-test-ast-conversion"]})

module React = {
  type element = Jsx.element
  @val external null: element = "null"
  type componentLike<'props, 'return> = Jsx.componentLike<'props, 'return>
  type component<'props> = Jsx.component<'props>
  external component: componentLike<'props, element> => component<'props> = "%component_identity"
  @module("react/jsx-runtime")
  external jsx: (component<'props>, 'props) => element = "jsx"
}

module Component = {
  @react.component
  let make = (~foo: string) => React.null
}

let _ = <Component bar="hello" />
