module AsProp = {
  type props = {
    @as("foo bar") fooBar: string,
    @as("data-x") dataX: string,
    @as("class") klass: string,
  }
  @react.componentWithProps
  let make = (_: props) => React.null
}
let asProp = <AsProp fooBar="a" dataX="b" klass="c" />

let sideEffect = ref(0)
let seqValue =
  <div
    title={
      sideEffect := 1
      "t"
    }
  />

let cond = ref(true)
let ternaryValue = <div title={cond.contents ? "a" : "b"} />

let fnValue = <div onClick={_ => sideEffect := 2} />

let styleValue = <div style={{color: "red"}} />

module WithElement = {
  @react.component
  let make = (~el: React.element) => el
}
let elementValue = <WithElement el={<span />} />

let optTitle: option<string> = Some("x")
let optNone: option<string> = None
let optionalSome = <div title=?optTitle />
let optionalNone = <div title=?optNone />
let optionalLiteralNone = <div title=?None />

let quoted = <div title={"a\"b}c"} />

let numberValue = <input tabIndex=3 />

// Prop value that is itself a sequence of lets
let blockValue =
  <div
    title={
      let a = "x"
      let b = a ++ "y"
      b
    }
  />

// Generic record with optional field explicitly undefined in source
module Opt = {
  @react.component
  let make = (~a: option<int>=?, ~b: string) => {
    ignore(a)
    <div title=b />
  }
}
let optComponent = <Opt b="x" />
let optComponentSome = <Opt a=1 b="x" />
let optComponentWithOpt = <Opt a=?Some(2) b="x" />

// A record with the runtime names "0", "1" is an array
module ArrayProps = {
  type props = {
    @as("0") first: string,
    @as("1") second: string,
  }
  @react.componentWithProps
  let make = (_: props) => React.null
}
let arrayProps = <ArrayProps first="a" second="b" />

// Prop value that is an await-free conditional element
let condChild = <div>{cond.contents ? <span /> : React.null}</div>
