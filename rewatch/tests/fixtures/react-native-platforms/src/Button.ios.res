open ReactNative

type token = int
let token = 42
let describe = token =>
  if token == 42 {
    "ios-token"
  } else {
    "invalid"
  }
let platform = "ios"
let enabled = false

module Details = {
  let label = "ios-details"

  @res.hoistedFunction
  let identify = value => value
}

@react.component
let make = (~title) =>
  <Button title={title ++ " on iOS"} color="#007aff" onPress={() => ()} />
