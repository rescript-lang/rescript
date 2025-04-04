@@jsxConfig({version: 4})

module V4A = {
  @react.component
  let make = (~a, ~b, _) => {
    Js.log("This function should be named 'TopLevel.react'")
    <div />
  }
}
