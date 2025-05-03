@@config({
  flags: ["-bs-jsx", "4", "-bs-jsx-preserve"],
})

module React = {
  type element = Jsx.element

  @val external null: element = "null"

  external float: float => element = "%identity"
  external int: int => element = "%identity"
  external string: string => element = "%identity"

  external array: array<element> => element = "%identity"

  type componentLike<'props, 'return> = Jsx.componentLike<'props, 'return>

  type component<'props> = Jsx.component<'props>

  external component: componentLike<'props, element> => component<'props> = "%identity"

  @module("react")
  external createElement: (component<'props>, 'props) => element = "createElement"

  @module("react")
  external cloneElement: (element, 'props) => element = "cloneElement"

  @module("react")
  external isValidElement: 'a => bool = "isValidElement"

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

  type fragmentProps = {children?: element}

  @module("react/jsx-runtime") external jsxFragment: component<fragmentProps> = "Fragment"
}

module ReactDOM = {
  external someElement: React.element => option<React.element> = "%identity"

  @module("react/jsx-runtime")
  external jsx: (string, JsxDOM.domProps) => Jsx.element = "jsx"

  @module("react/jsx-runtime")
  external jsxKeyed: (string, JsxDOM.domProps, ~key: string=?, @ignore unit) => Jsx.element = "jsx"

  @module("react/jsx-runtime")
  external jsxs: (string, JsxDOM.domProps) => Jsx.element = "jsxs"

  @module("react/jsx-runtime")
  external jsxsKeyed: (string, JsxDOM.domProps, ~key: string=?, @ignore unit) => Jsx.element =
    "jsxs"
}

module Icon = {
  @react.component
  let make = () => {
    <strong />
  }
}

let _ =
  <div>
    <h1> {React.string("Hello, world!")} </h1>
    <Icon />
  </div>
