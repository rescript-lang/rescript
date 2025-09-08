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

// ^int
