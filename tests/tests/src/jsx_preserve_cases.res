@@config({
  flags: ["-bs-jsx", "4", "-bs-jsx-preserve"],
})

// Printed as JSX or, when JSX can't express the element, as the call.
// tests/build_tests/jsx_preserve_semantics checks that both mean the same.

%%raw(`
function lowerComp(props) { return null }
`)

// Prop names that aren't JSX attribute names: printed as the call
module AsProp = {
  type props = {
    @as("foo bar") fooBar: string,
    @as("data-x") dataX: string,
  }
  @react.componentWithProps
  let make = (_: props) => React.null
}
let asProp = <AsProp fooBar="a" dataX="b" />

// A component bound to a lowercase global: <lowerComp /> would be an
// intrinsic element, so this is printed as the call
module Lower = {
  @react.component @val
  external make: (~x: int) => React.element = "lowerComp"
}
let lowerExternal = <Lower x=1 />

// Spreads that aren't variables, with and without a key
let base: JsxDOM.domProps = {title: "base"}
let getProps = (): JsxDOM.domProps => {title: "fn"}
type wrapper = {inner: JsxDOM.domProps}
let w = {inner: {title: "inner"}}
let spreadCall = <div {...getProps()} />
let spreadField = <div {...w.inner} />
let spreadKeyed = <div {...base} key="k" />
let spreadCallKeyed = <div {...getProps()} key="k" />

// A sequence is not an AssignmentExpression, so it needs parentheses
let count = ref(0)
let seqValue =
  <div
    title={
      count := 1
      "t"
    }
  />

// A single child of an optional children prop is printed without braces
let fragmentChild =
  <>
    <input />
  </>

// A spread of a small record without optional fields copies its fields
module Req = {
  type props = {a: string, b: string}
  @react.componentWithProps
  let make = (props: props) => <div title={props.a ++ props.b} />
}
let copySpread = (p: Req.props) => <Req {...p} b="override" />

// A tag that is a field of a local module stays a member expression
let localComp = () => {
  let comp = React.memo(Req.make)
  module M = {
    let make = comp
  }
  <M a="a" b="b" />
}

// An explicit undefined key is still passed to the runtime
let undefinedKey = <div key=?None />
