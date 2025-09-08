@react.component
let make = () => {
  let goToVerifyUser = RootProvider.useVerifyUser()

  <React.Fragment>
    <Rimble.Box p=1>
      <Rimble.Button onClick={_e => goToVerifyUser()}> {React.string("Profile")} </Rimble.Button>
    </Rimble.Box>
  </React.Fragment>
}
