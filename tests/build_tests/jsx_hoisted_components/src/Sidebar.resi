module Provider: {
  @react.component
  let make: (~children: React.element) => React.element
  let helper: unit => string
}

module Group: {
  module Provider: {
    @react.component
    let make: (~children: React.element) => React.element
  }
}

module WithProps: {
  type props = {children: React.element}
  let make: React.component<props>
}

let privateElement: React.element

module Memo: {
  @react.component
  let make: (~children: React.element) => React.element
}

module ForwardRef: {
  @react.component
  let make: (~children: React.element) => React.element
}

module WithPrivate: {
  @react.component
  let make: (~children: React.element) => React.element
}

module Sidebar: {
  @react.component
  let make: (~children: React.element) => React.element
}

module External: {
  @react.component
  let make: (~children: React.element) => React.element
}

module ExternalDebug: {
  // An existing, manually written primitive interface must still match.
  type props<'children> = {children: 'children}
  @module("node:util")
  external make: React.component<props<React.element>> = "inspect"
}

module ExternalManual: {
  type props<'children> = {children: 'children}
  @module("./external-component.js")
  external make: React.component<props<React.element>> = "default"
}

module ExternalAnnotated: {
  @react.component @module("node:util")
  external make: (~children: React.element) => React.element = "inspect"
}
