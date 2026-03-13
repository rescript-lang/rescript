type element

@val external null: element = "null"

external string: string => element = "%identity"

type componentLike<'props, 'return> = 'props => 'return

type component<'props> = componentLike<'props, element>

@module("react/jsx-runtime")
external jsx: (component<'props>, 'props) => element = "jsx"
