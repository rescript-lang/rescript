type token = int

let token = 42
let describe = token => token->Int.toString
let platform = "ios"

module Details = {
  let label = "ios-details"
}

describe(token)->ignore
// ^hov
