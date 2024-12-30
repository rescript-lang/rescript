@@jsxConfig({version: 4, mode: "classic"})

module V4C = {
  @react.component
  let make:
    type a. (~a: a, ~b: a, a) => React.element =
    (~a, ~b, _) => <div />
}

let v4c = <V4C a=1 b=2 />

@@jsxConfig({version: 4, mode: "automatic"})

module V4A = {
  @react.component
  let make:
    type a. (~a: a, ~b: a, a) => React.element =
    (~a, ~b, _) => <div />
}

let v4a = <V4A a=1 b=2 />
