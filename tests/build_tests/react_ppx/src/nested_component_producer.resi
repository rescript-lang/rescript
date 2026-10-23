module Provider: {
  @react.component
  let make: (~children: React.element) => React.element
}

module Trigger: {
  @react.component
  let make: (~label: string) => React.element
}

module Menu: {
  module Item: {
    @react.component
    let make: (~children: React.element) => React.element
  }
}

module Ext: {
  @react.component
  let make: (~children: React.element) => React.element
}

module rec RecA: {
  @react.component
  let make: unit => React.element
}
and RecB: {
  @react.component
  let make: unit => React.element
}

let hidden: React.element
