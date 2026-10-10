module Icon = {
  @react.component
  let make = () => <strong />
}

let ctx = React.createContext(0)

// Local module whose make is a call result used once (may be inlined into the tag)
let localProvider = () => {
  module P = {
    let make = React.Context.provider(ctx)
  }
  <P value=1>
    <div />
  </P>
}
let localProviderResult = localProvider()

let localMemoOnce = () => {
  module M = {
    let make = React.memo(Icon.make)
  }
  <M />
}
let localMemoOnceResult = localMemoOnce()

// Local function component referenced directly through a local module
let localFn = () => {
  let make = React.component((_: Icon.props) => <i />)
  module M = {
    let make = make
  }
  <M />
}
let localFnResult = localFn()

// Props record bound once and inlined
let inlinedProps = () => {
  let p: JsxDOM.domProps = {title: "a", className: "b"}
  <div {...p} />
}
let inlinedPropsResult = inlinedProps()

// Props bound once with a non-constant field
let dyn = ref("d")
let inlinedDynProps = () => {
  let p: JsxDOM.domProps = {title: dyn.contents}
  <div {...p} />
}
let inlinedDynPropsResult = inlinedDynProps()

// Element used through an inlined helper
let wrap = x => <section>{x}</section>
let wrapped = wrap(<span />)

// Element returned from a switch
let pick = n =>
  switch n {
  | 0 => <a />
  | _ => <b />
  }
let picked = pick(0)

// Key from a computation with side effects and props with side effects
let counter = ref(0)
let next = () => {
  counter := counter.contents + 1
  Int.toString(counter.contents)
}
let orderKeyed = <div key={next()} title={next()} />
let orderProps = <div title={next()} className={next()} />

// Tag from an external default import
module Default = {
  @react.component @module("some-lib")
  external make: (~x: int) => React.element = "default"
}
let defaultImport = <Default x=1 />

module LowerNamed = {
  @react.component @module("some-lib")
  external make: (~x: int) => React.element = "head"
}
let lowerNamed = <LowerNamed x=1 />
