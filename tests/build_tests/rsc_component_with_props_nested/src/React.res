type element

@val external null: element = "null"

external string: string => element = "%identity"

type componentLike<'props, 'return> = 'props => 'return

type component<'props>

external component: componentLike<'props, element> => component<'props> = "%component_identity"

@module("react/jsx-runtime")
external jsx: (component<'props>, 'props) => element = "jsx"
