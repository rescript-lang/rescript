let ax = 4
let _ = ax
let ax = ""
let _ = ax
module Test = {
  type t = {name: int}
  let add = (ax: t) => ax.name + 1
}
let ax: Test.t = {name: 4}
// ax->
//     ^com

// ax
//   ^com
