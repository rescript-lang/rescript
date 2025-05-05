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

let _single_element_child =
  <div>
    <h1> {React.string("Hello, world!")} </h1>
  </div>

let _multiple_element_children =
  <div>
    <h1> {React.string("Hello, world!")} </h1>
    <Icon />
  </div>

let _single_element_fragment =
  <>
    <input />
  </>

let _multiple_element_fragment =
  <>
    <input type_="text" />
    <input type_="number" />
  </>

let _unary_element_with_props = <input type_="text" className="foo" />

let _container_element_with_props_and_children =
  <div title="foo" className="foo"> {React.string("Hello, world!")} </div>

let baseProps: JsxDOM.domProps = {
  title: "foo",
  className: "foo",
}

let _unary_element_with_spread_props = <input {...baseProps} type_="text" />

let _container_with_spread_props =
  <div {...baseProps} title="barry" className="barry">
    {React.string("Hello, world!")}
    <input type_="text" />
  </div>

let baseChildren = React.array([
  <span> {React.string("Hello, world!")} </span>,
  <span> {React.string("Hello, world!")} </span>,
])

let _container_with_spread_children = <div title="barry" className="barry"> ...baseChildren </div>

let _container_with_spread_props_and_children =
  <div {...baseProps} title="barry" className="barry"> ...baseChildren </div>

let _unary_element_with_spread_props_keyed = <input {...baseProps} type_="text" key="barry-key" />

let _container_with_spread_props_keyed =
  <div {...baseProps} title="barry" className="barry" key="barry-key">
    {React.string("Hello, world!")}
    <input type_="text" />
  </div>

let _unary_element_with_only_spread_props = <input {...baseProps} />

// Simulate an external component
%%raw(`
  function QueryClientProvider(props) { return props.children }
  `)

module A = {
  @react.component
  external make: (~children: React.element) => React.element = "QueryClientProvider"
}

module B = {
  @react.component
  let make = () => {
    <p> {React.string("Hello, world!")} </p>
  }
}

let _external_component_with_children =
  <A>
    <strong />
    <B />
  </A>

module MyWeirdComponent = {
  type props = {\"MyWeirdProp": string}

  let make = props =>
    <p>
      {React.string("foo")}
      {React.string(props.\"MyWeirdProp")}
    </p>
}

let _escaped_jsx_prop = <MyWeirdComponent \"MyWeirdProp"="bar" />
