let base: JsxDOM.domProps = {title: "base", className: "base"}
let getProps = (): JsxDOM.domProps => {title: "fn"}
type wrapper = {inner: JsxDOM.domProps}
let w = {inner: {title: "inner"}}

let spreadVar = <div {...base} />
let spreadVarOverride = <div {...base} title="o" />
let spreadCall = <div {...getProps()} />
let spreadCallOverride = <div {...getProps()} title="o" />
let spreadField = <div {...w.inner} />
let spreadFieldOverride = <div {...w.inner} title="o" />
let spreadChildren =
  <div {...base}>
    <span />
    <b />
  </div>
let spreadKeyed = <div {...base} key="k" title="o" />
let spreadOnlyKeyed = <div {...base} key="k" />
let spreadCallKeyed = <div {...getProps()} key="k" />

// Spread of a locally built constant record (inlinable)
let localConst = () => {
  let p: JsxDOM.domProps = {title: "local"}
  <div {...p} className="c" />
}
let localConstResult = localConst()

let localConstOnly = () => {
  let p: JsxDOM.domProps = {title: "local"}
  <div {...p} />
}
let localConstOnlyResult = localConstOnly()

// Spread record built with record spread syntax
let localSpread = () => {
  let p: JsxDOM.domProps = {...base, title: "x"}
  <div {...p} />
}
let localSpreadResult = localSpread()

// Spread inside a component of its props parameter
module Pass = {
  type props = {title?: string, className?: string}
  @react.componentWithProps
  let make = (props: props) => <div title=?props.title className=?props.className />
}
module Wrap = {
  @react.componentWithProps
  let make = (props: Pass.props) => <Pass {...props} className="wrapped" />
}
let wrap = <Wrap title="t" />

// Spread of a record value coming from an optional
let fromOpt = (o: option<JsxDOM.domProps>) =>
  switch o {
  | Some(p) => <div {...p} />
  | None => React.null
  }
let fromOptResult = fromOpt(Some({title: "o"}))

// An optional prop that is None overrides the spread's value
let spreadNone = <div {...base} title=?None />
let optNone: option<string> = None
let spreadOptNone = <div {...base} title=?optNone />

// Spread with children prop in spread and explicit children
let childrenBase: JsxDOM.domProps = {children: React.string("from spread")}
let spreadWithChildren = <div {...childrenBase}>{React.string("explicit")}</div>
let spreadChildrenNone = <div {...childrenBase} children=?None />
