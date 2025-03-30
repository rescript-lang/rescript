type status = On | Off

@@jsxConfig({version: 4})

@react.component
let make = (~status: status, ~name: string) => {
  ignore(status)
  ignore(name)
  React.null
}
