module Provider = {
  @react.component
  let make = (~children: React.element) => children
}

module Trigger = {
  @react.component
  let make = (~label: string) => React.string(label)

  let make = React.memo(make)
}

module Menu = {
  module Item = {
    @react.component
    let make = (~children: React.element) => children
  }
}

module Ext = {
  @react.component @module("./ext.js")
  external make: (~children: React.element) => React.element = "default"
}

module Hidden = {
  @react.component
  let make = () => React.null
}

module rec RecA: {
  @react.component
  let make: unit => React.element
} = {
  @react.component
  let make = () => <RecB />
}
and RecB: {
  @react.component
  let make: unit => React.element
} = {
  @react.component
  let make = () => React.null
}

let hidden = <Hidden />
