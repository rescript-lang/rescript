type token = {name: string}

let token = {name: "android-token"}
let describe = token => token.name
let platform = "android"

module Details = {
  let label = "android-details"
}

describe(token)->ignore
// ^hov
