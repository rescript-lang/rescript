module React = {
  type element = Jsx.element
  @val external null: element = "null"
  type componentLike<'props, 'return> = Jsx.componentLike<'props, 'return>
  type component<'props> = Jsx.component<'props>

  external component: componentLike<'props, element> => component<'props> = "%component_identity"
  @module("react/jsx-runtime")
  external jsx: (component<'props>, 'props) => element = "jsx"
}

module List = {
  type separatorProps = {index: int}

  @react.component
  let make = (~itemSeparatorComponent: React.component<separatorProps>) => React.null
}

let _ = <List itemSeparatorComponent={_props => React.null} />
