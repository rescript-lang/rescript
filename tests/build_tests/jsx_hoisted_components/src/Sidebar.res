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
  let make = () => React.string("private")
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
