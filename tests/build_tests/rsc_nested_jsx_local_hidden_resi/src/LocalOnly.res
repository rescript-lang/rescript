module Hidden = {
  @react.component
  let make = () => React.null
}

@react.component
let make = () => {
  <Hidden />
}
