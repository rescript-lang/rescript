@@jsxConfig({version: 4})

module C0 = {
  module M = {
    @react.component
    let make = () => React.null
  }

  module type S = module type of M

  @react.component
  let make = (~component as module(C: S)=module(M: S)) => <C />
}
