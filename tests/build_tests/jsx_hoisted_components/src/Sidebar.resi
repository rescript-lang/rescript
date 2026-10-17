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
