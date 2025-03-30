@@jsxConfig({version: 4})

module V4A = {
  @react.component
  let make:
    type a. (~a: a, ~b: a, a) => React.element =
    (~a, ~b, _) => <div />
}

let v4a = <V4A a=1 b=2 />
