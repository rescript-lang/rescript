%%raw(`
function lowerComp(props) { return null }
`)

module Icon = {
  @react.component
  let make = () => <strong />
}

// Local module component inside a function
let localModule = () => {
  module L = {
    @react.component
    let make = () => <i />
  }
  <L />
}
let localModuleResult = localModule()

// Local module whose make is a memo
let localMemo = () => {
  module L = {
    @react.component
    let make = () => <i />
    let make = React.memo(make)
  }
  <L />
}
let localMemoResult = localMemo()

// External bound to a lowercase global
module Lower = {
  @react.component @val
  external make: (~x: int) => React.element = "lowerComp"
}
let lowerExternal = <Lower x=1 />

// Module alias
module I = Icon
let aliased = <I />

// Functor-produced component
module F = (X: {}) => {
  @react.component
  let make = () => <b />
}
module G = F()
let functor = <G />

// First-class component stored in a module
let iconMake = Icon.make
module C = {
  let make = iconMake
}
let firstClass = <C />

// Component passed as a function argument and rebound in a local module
let withComponent = (comp: React.component<Icon.props>) => {
  module M = {
    let make = comp
  }
  <M />
}
let withComponentResult = withComponent(Icon.make)

// React.Fragment component with key
let keyedFragment =
  <React.Fragment key="k">
    <div />
  </React.Fragment>
let plainReactFragment =
  <React.Fragment>
    <div />
    <span />
  </React.Fragment>

// Context provider
let ctx = React.createContext(0)
module Provider = {
  let make = React.Context.provider(ctx)
}
let provider =
  <Provider value=1>
    <div />
  </Provider>

// Component defined at top level in a nested module of a nested module
module Outer = {
  module Inner = {
    @react.component
    let make = (~a) => <div title=a />
  }
}
let nested = <Outer.Inner a="x" />
