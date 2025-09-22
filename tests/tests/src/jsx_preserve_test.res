@@config({
  flags: ["-bs-jsx", "4", "-bs-jsx-preserve"],
})

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

  @react.componentWithProps
  let make = props =>
    <p>
      {React.string("foo")}
      {React.string(props.\"MyWeirdProp")}
    </p>
}

let _escaped_jsx_prop = <MyWeirdComponent \"MyWeirdProp"="bar" />

let _large_component =
  <div title="foo" className="bar" tabIndex={1} onClick={_ => ()} onMouseDown={_ => ()}>
    <p title="foo" className="bar" tabIndex={1} onClick={_ => ()} onMouseDown={_ => ()}>
      {React.string("Hello, world!")}
    </p>
    <strong title="foo" className="bar" tabIndex={1} onClick={_ => ()} onMouseDown={_ => ()}>
      {React.string("Hello, world!")}
    </strong>
    <p> {React.int(5)} </p>
  </div>

module ComponentWithOptionalProps = {
  @react.component
  let make = (
    ~i as _: option<int>=?,
    ~s as _: option<string>=?,
    ~element as _: option<React.element>=?,
  ) => React.null
}

let _optional_props = <ComponentWithOptionalProps i=1 s="test" element={<div />} />

let _props_with_hyphen = <label ariaLabel={"close sidebar"} dataTestId="test" />

let _fragment = <> {Jsx.string("Hello, world!")} </>

let _youtube_iframe =
  <iframe
    width="1911"
    height="1075"
    src="https://www.youtube.com/embed/dQw4w9WgXcQ"
    frameBorder={0}
    allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share"
    referrerPolicy="strict-origin-when-cross-origin"
    allowFullScreen={true}
  >
  </iframe>

module X = {
  type props = {}
  let make = (_props: props) => React.null
}

let _ = <X />

module Y = {
  let x = 42

  @react.component
  let make = () => React.null

  let make = React.memo(make)
}

let _ = <Y />

let context = React.createContext(0)

module ContextProvider = {
  let make = React.Context.provider(context)
}

@react.component
let make = (~children) => {
  <ContextProvider value=42> {children} </ContextProvider>
}
