// The boolean is deliberately eligible for cross-module constant propagation.
let enabled = Button.enabled
let platform = Button.platform
let describe = () => Button.describe(Button.token)
let details = Button.Details.label
let identify = () => Button.Details.identify(Button.Details.label)
@react.component
let make = () => <Button title="Platform button" />
