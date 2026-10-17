@@jsxConfig({version: 4})

module M4 = {
  /** Doc Comment For M4 */
  @react.component
  let make = (~first, ~fun="", ~second="") => React.string(first ++ fun ++ second)
}

let _ = <M4 first="abc" />
//       ^def

// <M4 first="abc" f
//                  ^com

let _ = <M4 first="abc" />
//       ^hov

module MM = {
  @react.component
  let make = () => React.null
}

module Other = {
  @react.component
  let make = (~name) => React.string(name)
}

module External = {
  @react.component @module("./client.js")
  external make: (~children: React.element) => React.element = "default"
}

ignore(<External>{React.string("child")}</External>)
//      ^hov
ignore(<External>{React.string("child")}</External>)
//      ^def

// ^int
