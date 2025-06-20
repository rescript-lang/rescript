/* First time reading a ReScript file? */
/* `external` is the foreign function call in OCaml. */
/* here we're saying `I guarantee that on the JS side, we have a `render` function in the module "react-dom"
 that takes in a reactElement, a dom element, and returns unit (nothing) */
/* It's like `let`, except you're pointing the implementation to the JS side. The compiler will inline these
 calls and add the appropriate `require("react-dom")` in the file calling this `render` */

// Helper so that ReactDOM itself doesn't bring any runtime
@val @return(nullable)
external querySelector: string => option<Dom.element> = "document.querySelector"

module Client = {
  module Root = {
    type t

    @send external render: (t, React.element) => unit = "render"

    @send external unmount: (t, unit) => unit = "unmount"
  }

  @module("react-dom/client")
  external createRoot: Dom.element => Root.t = "createRoot"

  @module("react-dom/client")
  external hydrateRoot: (Dom.element, React.element) => Root.t = "hydrateRoot"
}

@module("react-dom")
external createPortal: (React.element, Dom.element) => React.element = "createPortal"

external domElementToObj: Dom.element => {..} = "%identity"

type style = ReactDOMStyle.t

type domRef = JsxDOM.domRef

module Ref = {
  type t = domRef
  type currentDomRef = React.ref<Js.nullable<Dom.element>>
  type callbackDomRef = Js.nullable<Dom.element> => unit

  external domRef: currentDomRef => domRef = "%identity"
  external callbackDomRef: callbackDomRef => domRef = "%identity"
}

type domProps = JsxDOM.domProps

@variadic @module("react")
external createElement: (string, ~props: domProps=?, array<React.element>) => React.element =
  "createElement"

@variadic @module("react")
external createDOMElementVariadic: (
  string,
  ~props: domProps=?,
  array<React.element>,
) => React.element = "createElement"

external someElement: React.element => option<React.element> = "%identity"

@module("react/jsx-runtime")
external jsx: (string, JsxDOM.domProps) => Jsx.element = "jsx"

@module("react/jsx-runtime")
external jsxKeyed: (string, JsxDOM.domProps, ~key: string=?, @ignore unit) => Jsx.element = "jsx"

@module("react/jsx-runtime")
external jsxs: (string, JsxDOM.domProps) => Jsx.element = "jsxs"

@module("react/jsx-runtime")
external jsxsKeyed: (string, JsxDOM.domProps, ~key: string=?, @ignore unit) => Jsx.element = "jsxs"

module Style = ReactDOMStyle
