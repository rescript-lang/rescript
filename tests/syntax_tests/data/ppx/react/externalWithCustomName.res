@@jsxConfig({version: 4})

module Foo = {
  @react.component @module("Foo")
  external component: (~a: int, ~b: string, _) => React.element = "component"
}

let t = <Foo.component a=1 b={"1"} />
