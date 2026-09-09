type token
let token: token
let describe: token => string
let platform: string
let enabled: bool
@react.component
let make: (~title: string) => React.element
module Details: {
  let label: string
  let identify: string => string
}
