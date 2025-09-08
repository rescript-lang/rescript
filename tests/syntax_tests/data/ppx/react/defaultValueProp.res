module C0 = {
  @react.component
  let make = (~a=2, ~b=a*2) => React.int(a + b)
}

module C1 = {
  @react.component
  let make = (~a=2, ~b) => React.int(a + b)
}

module C2 = {
  let a = "foo"
  @react.component
  let make = (~a=a) => React.string(a)
}

module C3 = {
  @react.component
  let make = (~disabled as everythingDisabled: bool=false) => {
    React.string(everythingDisabled ? "true" : "false")
  }
}