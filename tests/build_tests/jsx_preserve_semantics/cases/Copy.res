module Req = {
  type props = {a: string, b: string}
  @react.componentWithProps
  let make = (props: props) => <div title={props.a ++ props.b} />
}

let base: Req.props = {a: "a", b: "b"}
let copySpread = <Req {...base} b="override" />

let getBase = (): Req.props => {a: "fa", b: "fb"}
let copySpreadCall = <Req {...getBase()} b="override" />

let copyInFn = (p: Req.props) => <Req {...p} a="x" />
let copyInFnResult = copyInFn(base)
