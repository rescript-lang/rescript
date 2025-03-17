@@jsxConfig({version: 4})

module C4A0 = {
  @react.component
  let make =
    (@as("open") ~_open, @as("type") ~_type: string) => React.string(_open)
}
module C4A1 = {
  @react.component
  external make: (@as("open") ~_open: string, @as("type") ~_type: string) => React.element =
    "default"
}

let c4a0 = <C4A0 _open="x" _type="t" />
let c4a1 = <C4A1 _open="x" _type="t" />
