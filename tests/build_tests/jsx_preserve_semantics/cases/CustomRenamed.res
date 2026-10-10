@@jsxConfig({version: 4, module_: "MyJsx"})

module MyJsx = {
  type element = Jsx.element
  type component<'props> = Jsx.component<'props>
  external component: Jsx.componentLike<'props, element> => component<'props> =
    "%component_identity"

  @module("some-runtime")
  external jsx: (component<'props>, 'props) => element = "j"
  @module("some-runtime")
  external jsxs: (component<'props>, 'props) => element = "js"
  @module("some-runtime")
  external jsxKeyed: (component<'props>, 'props, ~key: string=?, @ignore unit) => element = "j"
  @module("some-runtime")
  external jsxsKeyed: (component<'props>, 'props, ~key: string=?, @ignore unit) => element = "js"
  external array: array<element> => element = "%identity"
  type fragmentProps = {children?: element}
  @module("some-runtime") external jsxFragment: component<fragmentProps> = "Frag"

  type domProps = {children?: element, title?: string}
  module Elements = {
    external someElement: element => option<element> = "%identity"
    @module("some-runtime")
    external jsx: (string, domProps) => element = "j"
    @module("some-runtime")
    external jsxs: (string, domProps) => element = "js"
    @module("some-runtime")
    external jsxKeyed: (string, domProps, ~key: string=?, @ignore unit) => element = "j"
    @module("some-runtime")
    external jsxsKeyed: (string, domProps, ~key: string=?, @ignore unit) => element = "js"
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
let emptyFragment = <></>
let component = <Comp title="t" />
let keyed = <div key="k" />

let localComp = () => {
  let comp = Comp.make
  module M = {
    let make = comp
  }
  <M title="m" />
}
let localCompResult = localComp()

module Top = {
  let make = Comp.make
}
let top = <Top title="top" />

@module("react") external memo: MyJsx.component<'p> => MyJsx.component<'p> = "memo"
let memoComp = memo(Comp.make)
module TopMemo = {
  let make = memoComp
}
let topMemo = <TopMemo title="tm" />

let localMemo = () => {
  let comp = memo(Comp.make)
  module M = {
    let make = comp
  }
  <M title="lm" />
}
let localMemoResult = localMemo()
