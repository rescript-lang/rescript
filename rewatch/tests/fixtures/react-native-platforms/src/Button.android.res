open ReactNative

type token = {name: string}
let token = {name: "android-token"}
let describe = token => token.name
let platform = "android"
let enabled = true

module Details = {
  let label = "android-details"

  @res.hoistedFunction
  let identify = value => value
}

@react.component
let make = (~title) =>
  <Button title={title ++ " on Android"} color="#3ddc84" onPress={() => ()} />
