@@jsxConfig({version: 4})

module V4A = {
  module FancyInput = {
    @react.component
    let make = React.forwardRef((~className=?, ~children, ref) =>
      <div>
        <input
          type_="text"
          ?className
          ref=?{Nullable.toOption(ref)->Belt.Option.map(ReactDOM.Ref.domRef)}
        />
        children
      </div>
    )
  }

  @react.component
  let make = () => {
    let input = React.useRef(Nullable.null)

    <div>
      <FancyInput ref=input> {React.string("Click to focus")} </FancyInput>
    </div>
  }
}

module V4AUncurried = {
  module FancyInput = {
    @react.component
    let make = React.forwardRef((~className=?, ~children, ref) =>
      <div>
        <input
          type_="text"
          ?className
          ref=?{Nullable.toOption(ref)->Belt.Option.map(ReactDOM.Ref.domRef)}
        />
        children
      </div>
    )
  }

  @react.component
  let make = () => {
    let input = React.useRef(Nullable.null)

    <div>
      <FancyInput ref=input> {React.string("Click to focus")} </FancyInput>
    </div>
  }
}
