@@directive("'use client';")

module Provider = {
  @react.component
  let make = (~children) => children

  let helper = () => "helper"
}

module Group = {
  module Provider = {
    @react.component
    let make = (~children) => children
  }
}

module WithProps = {
  type props = {children: React.element}

  @react.componentWithProps
  let make = props => props.children
}

module Hidden = {
  @react.component
  let make = (~children=React.string("private")) => children
}

module WithPrivate = {
  @react.component
  let make = (~children) => <Hidden>{children}</Hidden>
}

module Sidebar = {
  @react.component
  let make = (~children) => children
}

module External = {
  @react.component @module("./external-component.js")
  external make: (~children: React.element) => React.element = "default"
}

module ExternalDebug = {
  @react.component @module("node:util")
  external make: (~children: React.element) => React.element = "inspect"
}

module ExternalManual = {
  @react.component @module("./external-component.js")
  external make: (~children: React.element) => React.element = "default"
}

module ExternalAnnotated = {
  @react.component @module("node:util")
  external make: (~children: React.element) => React.element = "inspect"
}

module HiddenExternal = {
  @react.component @module("./unused-private-component.js")
  external make: (~children: React.element) => React.element = "default"

  @module("./jsx-runtime.js") external effect: unit => unit = "effect"
  effect()
}

module Memo = {
  @react.component
  let make = React.memo((~children) => children)
}

module ForwardRef = {
  @react.component
  let make = React.forwardRef((~children, _ref) => children)
}

let privateElement = <Hidden />
