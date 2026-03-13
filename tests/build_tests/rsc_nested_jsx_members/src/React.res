type element

@val external null: element = "null"

external string: string => element = "%identity"

external array: array<element> => element = "%identity"

type componentLike<'props, 'return> = 'props => 'return

type component<'props> = componentLike<'props, element>

@module("react")
external createElement: (component<'props>, 'props) => element = "createElement"

@variadic @module("react")
external createElementVariadic: (component<'props>, 'props, array<element>) => element =
  "createElement"

@module("react/jsx-runtime")
external jsx: (component<'props>, 'props) => element = "jsx"

@module("react/jsx-runtime")
external jsxKeyed: (component<'props>, 'props, ~key: string=?, @ignore unit) => element = "jsx"

@module("react/jsx-runtime")
external jsxs: (component<'props>, 'props) => element = "jsxs"

@module("react/jsx-runtime")
external jsxsKeyed: (component<'props>, 'props, ~key: string=?, @ignore unit) => element = "jsxs"
