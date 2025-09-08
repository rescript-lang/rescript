@@jsxConfig({version: 4, mode: "classic"})

module V4C: {
  module FancyInput: {
    @react.component
    let make: (
      ~className: string=?,
      ~children: React.element,
      ~ref: ReactDOM.Ref.currentDomRef=?,
    ) => React.element
  }

  module ForwardRef: {
    @react.component
    let make: (~ref: React.ref<Js.Nullable.t<inputRef>>=?) => React.element
  }
}

module V4CUncurried: {
  module FancyInput: {
    @react.component
    let make: (
      ~className: string=?,
      ~children: React.element,
      ~ref: ReactDOM.Ref.currentDomRef=?,
    ) => React.element
  }

  module ForwardRef: {
    @react.component
    let make: (~ref: React.ref<Js.Nullable.t<inputRef>>=?) => React.element
  }
}

@@jsxConfig({version: 4, mode: "automatic"})

module V4A: {
  module FancyInput: {
    @react.component
    let make: (
      ~className: string=?,
      ~children: React.element,
      ~ref: ReactDOM.Ref.currentDomRef=?,
    ) => React.element
  }

  module ForwardRef: {
    @react.component
    let make: (~ref: React.ref<Js.Nullable.t<inputRef>>=?) => React.element
  }
}

module V4AUncurried: {
  module FancyInput: {
    @react.component
    let make: (
      ~className: string=?,
      ~children: React.element,
      ~ref: ReactDOM.Ref.currentDomRef=?,
    ) => React.element
  }

  module ForwardRef: {
    @react.component
    let make: (~ref: React.ref<Js.Nullable.t<inputRef>>=?) => React.element
  }
}
