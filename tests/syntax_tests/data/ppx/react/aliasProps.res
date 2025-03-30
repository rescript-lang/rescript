@@jsxConfig({version: 4})

module C0 = {
  @react.component
  let make = (~priority as _, ~text="Test") => React.string(text)
}

module C1 = {
  @react.component
  let make = (~priority as p, ~text="Test") => React.string(p ++ text)
}

module C2 = {
  @react.component
  let make = (~foo as bar="") => React.string(bar)
}

module C3 = {
  @react.component
  let make = (~foo as bar="", ~a=bar, ~b) => {
    React.string(bar ++ a ++ b)
  }
}

module C4 = {
  @react.component
  let make = (~a as b, ~x=true) => <div> b </div>
}

module C5 = {
  @react.component
  let make = (~a as (x, y), ~z=3) => x + y + z
}

module C6 = {
  module type Comp = {
    @react.component
    let make: unit => React.element
  }

  @react.component
  let make = (~comp as module(Comp: Comp), ~x as (a, b)) => <Comp />
}
