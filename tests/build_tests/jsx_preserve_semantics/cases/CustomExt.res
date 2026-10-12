@@jsxConfig({version: 4, module_: "MyJsx"})

module MyJsx = {
  type element = Jsx.element
  type component<'props> = Jsx.component<'props>
  external component: Jsx.componentLike<'props, element> => component<'props> =
    "%component_identity"

  @module("react/jsx-runtime")
  external jsx: (component<'props>, 'props) => element = "jsx"
  @module("react/jsx-runtime")
  external jsxs: (component<'props>, 'props) => element = "jsxs"
  @module("react/jsx-runtime")
  external jsxKeyed: (component<'props>, 'props, ~key: string=?, @ignore unit) => element = "jsx"
  @module("react/jsx-runtime")
  external jsxsKeyed: (component<'props>, 'props, ~key: string=?, @ignore unit) => element = "jsxs"
  external array: array<element> => element = "%identity"
  type fragmentProps = {children?: element}
  @module("react/jsx-runtime") external jsxFragment: component<fragmentProps> = "Fragment"

  type domProps = {children?: element, title?: string}
  module Elements = {
    external someElement: element => option<element> = "%identity"
    @module("react/jsx-runtime")
    external jsx: (string, domProps) => element = "jsx"
    @module("react/jsx-runtime")
    external jsxs: (string, domProps) => element = "jsxs"
    @module("react/jsx-runtime")
    external jsxKeyed: (string, domProps, ~key: string=?, @ignore unit) => element = "jsx"
    @module("react/jsx-runtime")
    external jsxsKeyed: (string, domProps, ~key: string=?, @ignore unit) => element = "jsxs"
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
let fragmentSingle =
  <>
    <span />
  </>
let emptyFragment = <> </>
let component = <Comp title="t" />
let keyed = <div key="k" />
