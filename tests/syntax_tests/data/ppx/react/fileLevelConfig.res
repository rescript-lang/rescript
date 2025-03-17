@@jsxConfig({version: 4})

module V4A = {
  @react.component
  let make = (~msg) => {
    <div> {msg->React.string} </div>
  }
}
