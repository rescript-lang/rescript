@@jsxConfig({version:4, mode: "classic"})

module V4C1 = {
  @react.component(:sharedProps<string>)
  let make = (~x, ~y) => React.string(x ++ y)
}

module V4C2 = {
  @react.component(:sharedProps<'a>)
  let make = (~x, ~y) => React.string(x ++ y)
}

module V4C3 = {
  @react.component(:sharedProps<string, 'a>)
  let make = (~x, ~y) => React.string(x ++ y)
}

module V4C4 = {
  @react.component(:sharedProps)
  let make = (~x, ~y) => React.string(x ++ y)
}

module V4C5 = {
  @react.component(:sharedProps<string>)
  external make: (~x: string, ~y: 'a) => React.element = "default"
}

module V4C6 = {
  @react.component(:sharedProps<'a>)
  external make: (~x: string, ~y: 'a) => React.element = "default"
}

module V4C7 = {
  @react.component(:sharedProps<string, 'a>)
  external make: (~x: string, ~y: string) => React.element = "default"
}

module V4C8 = {
  @react.component(:sharedProps)
  external make: (~x: string, ~y: string) => React.element = "default"
}

module V4C9 = {
  @react.component(:sharedProps)
  let make = (~x: string, ~y: int) => React.string(x ++ y)
}

@@jsxConfig({version:4, mode: "automatic"})

module V4A1 = {
  @react.component(:sharedProps<string>)
  let make = (~x, ~y) => React.string(x ++ y)
}

module V4A2 = {
  @react.component(:sharedProps<'a>)
  let make = (~x, ~y) => React.string(x ++ y)
}

module V4A3 = {
  @react.component(:sharedProps<string, 'a>)
  let make = (~x, ~y) => React.string(x ++ y)
}

module V4A4 = {
  @react.component(:sharedProps)
  let make = (~x, ~y) => React.string(x ++ y)
}

module V4A5 = {
  @react.component(:sharedProps<string>)
  external make: (~x: string, ~y: 'a) => React.element = "default"
}

module V4A6 = {
  @react.component(:sharedProps<'a>)
  external make: (~x: string, ~y: 'a) => React.element = "default"
}

module V4A7 = {
  @react.component(:sharedProps<string, 'a>)
  external make: (~x: string, ~y: string) => React.element = "default"
}

module V4A8 = {
  @react.component(:sharedProps)
  external make: (~x: string, ~y: string) => React.element = "default"
}

module V4A9 = {
  @react.component(:sharedProps)
  let make = (~x: string, ~y: int) => React.string(x ++ y)
}