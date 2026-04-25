module React = {
  type element = Jsx.element
  @val external null: element = "null"
  external string: string => element = "%identity"
  type componentLike<'props, 'return> = Jsx.componentLike<'props, 'return>
  type component<'props> = Jsx.component<'props>
  external component: componentLike<'props, element> => component<'props> = "%identity"
  @module("react")
  external createElement: (component<'props>, 'props) => element = "createElement"
}

@react.component
let rec make = (~foo) => React.createElement(make, {foo: foo})
