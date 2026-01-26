/* Implementation to satisfy interface build for tests */

external arr: Js.Array.t<int> = "arr"
external reT: Js.Re.t = "re"
external json: Js.Json.t = "json"
external nestedArr: Js.Array.t<Js.Re.t> = "nestedArr"

external useSet: Js.Set.t<int> => unit = "useSet"
