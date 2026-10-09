@@jsxConfig({version: 4, module_: "MyJsx"})

// A custom JSX module implemented with ReScript functions (not externals)
module MyJsx = {
  type element = Jsx.element
  type component<'props> = Jsx.component<'props>
  external component: Jsx.componentLike<'props, element> => component<'props> =
    "%component_identity"

  @module("react/jsx-runtime")
  external rawJsx: (component<'props>, 'props) => element = "jsx"
  @module("react/jsx-runtime")
  external rawJsxs: (component<'props>, 'props) => element = "jsxs"
  @module("react/jsx-runtime")
  external rawStrJsx: (string, 'props) => element = "jsx"
  @module("react/jsx-runtime")
  external rawStrJsxs: (string, 'props) => element = "jsxs"

  let jsx = (c, p) => rawJsx(c, p)
  let jsxs = (c, p) => rawJsxs(c, p)
  let jsxKeyed = (c, p, ~key: option<string>=?, ()) => {
    ignore(key)
    rawJsx(c, p)
  }
  let jsxsKeyed = (c, p, ~key: option<string>=?, ()) => {
    ignore(key)
    rawJsxs(c, p)
  }
  external array: array<element> => element = "%identity"
  type fragmentProps = {children?: element}
  @module("react/jsx-runtime") external jsxFragment: component<fragmentProps> = "Fragment"

  type domProps = {children?: element, title?: string}
  module Elements = {
    external someElement: element => option<element> = "%identity"
    let jsx = (s: string, p: domProps) => rawStrJsx(s, p)
    let jsxs = (s: string, p: domProps) => rawStrJsxs(s, p)
    let jsxKeyed = (s: string, p: domProps, ~key: option<string>=?, ()) => {
      ignore(key)
      rawStrJsx(s, p)
    }
    let jsxsKeyed = (s: string, p: domProps, ~key: option<string>=?, ()) => {
      ignore(key)
      rawStrJsxs(s, p)
    }
  }
}

module Comp = {
  @jsx.component
  let make = (~title) => <div title />
}

let element =
  <div title="x">
    <span />
  </div>
let fragment =
  <>
    <span />
    <b />
  </>
let component = <Comp title="t" />
let keyed = <div key="k" />
