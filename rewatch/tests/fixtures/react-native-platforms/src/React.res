// Minimal bindings keep the fixture independent of third-party binding packages.
type element
type componentLike<'props, 'return> = 'props => 'return
type component<-'props>

external component: componentLike<'props, element> => component<'props> = "%component_identity"

@module("react/jsx-runtime")
external jsx: (component<'props>, 'props) => element = "jsx"

@module("react/jsx-runtime")
external jsxKeyed: (component<'props>, 'props, ~key: string=?, @ignore unit) => element = "jsx"

@module("react/jsx-runtime")
external jsxs: (component<'props>, 'props) => element = "jsxs"

@module("react/jsx-runtime")
external jsxsKeyed: (component<'props>, 'props, ~key: string=?, @ignore unit) => element = "jsxs"
