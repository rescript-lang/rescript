type t = string

let createRescriptBlock = "CreateRescriptBlock"

let curry = "Curry"

let propTypes = "PropTypes"

let react = "React"

let reasonPervasives = "ReasonPervasives"
let reasonReact = "ReasonReact"

let forBsFile = s => s ++ "BS"

let forInnerModule = (~fileName, ~innerModuleName) =>
  (fileName->forBsFile) ++ ("." ++ innerModuleName)
let fromStringUnsafe = s => s
let toString = s => s
let compare = (s1: string, s2) => compare(s1, s2)

let uncapitalize = String.uncapitalize_ascii
