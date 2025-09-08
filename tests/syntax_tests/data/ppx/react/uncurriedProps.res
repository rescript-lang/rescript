@@jsxConfig({ version: 4 })

@react.component
let make = (~a: (. unit) => unit=(. ) => ()) => React.null

let func = (~callback: (. string, bool, bool) => unit=(. _, _, _) => (), ()) => {
  let _ = callback
}

func(~callback=(. str, a, b) => {
  let _ = str
  let _ = a
  let _ = b
}, ())

module Foo = {
  @react.component
  let make = (~callback: (. string, bool, bool) => unit=(. _, _, _) => ()) => {
    React.null
  }
}

module Bar = {
  @react.component
  let make = () => <Foo callback={(. _, _, _) => ()} />
}
