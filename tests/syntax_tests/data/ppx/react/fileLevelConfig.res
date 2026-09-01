@@jsxConfig({version: 4, module_: `Pre\x61ct`})

module V4A = {
  @react.component
  let make = (~msg) => {
    <div> {msg->React.string} </div>
  }
}
