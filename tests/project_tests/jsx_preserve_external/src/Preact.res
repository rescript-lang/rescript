type element = Jsx.element
// Custom JSX bindings must use the abstract component type, as React does.
// A function alias here lets function-style externals through and can produce
// invalid preserved JSX, as in the original bindings from issue #8047.
type component<'props> = Jsx.component<'props>

@module("preact/jsx-runtime")
external jsx: (component<'props>, 'props) => element = "jsx"

@module("preact/jsx-runtime")
external jsxs: (component<'props>, 'props) => element = "jsxs"

type domProps = {children?: element}

module Elements = {
  external someElement: element => option<element> = "%identity"

  @module("preact/jsx-runtime")
  external jsx: (string, domProps) => element = "jsx"

  @module("preact/jsx-runtime")
  external jsxs: (string, domProps) => element = "jsxs"
}
