/* Migration tests for interface (.resi) files using stdlib deprecations */

// Type alias migrations exercised via externals
external arr: Js.Array.t<int> = "arr"
external reT: Js.Re.t = "re"
external json: Js.Json.t = "json"
external nestedArr: Js.Array.t<Js.Re.t> = "nestedArr"

// Function type using a deprecated alias
external useSet: Js.Set.t<int> => unit = "useSet"
