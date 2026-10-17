@@jsxConfig({version: 4})

module Client: {
  @react.component @module("client")
  external make: (~children: React.element) => React.element = "Client"
}

module type Client = {
  @react.component @module("client")
  external make: (~children: React.element) => React.element = "Client"
}
