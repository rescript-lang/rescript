module React = {
  type element = Jsx.element
  @val external null: element = "null"
  type componentLike<'props, 'return> = Jsx.componentLike<'props, 'return>
  type component<'props> = Jsx.component<'props>
  external component: componentLike<'props, element> => component<'props> = "%identity"
  @module("react/jsx-runtime")
  external jsx: (component<'props>, 'props) => element = "jsx"
}

module Level2 = {
  @react.component
  let make = (~someProp: float) => {
    React.null
  }
}

module Level1 = {
  @react.component
  let make = (~someProp: array<int>) => {
    <Level2 someProp />
  }
}
