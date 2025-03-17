@@jsxConfig({version: 4})

module V4C = {
  @module("componentForwardRef") @react.component
    external make: (
      ~x: string,
      ~ref: ReactDOM.Ref.currentDomRef=?,
    ) => React.element = "component"
}
