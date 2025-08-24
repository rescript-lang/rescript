external someJson: Js_json.t = "someJson"
external strToJson: string => Js_json.t = "strToJson"

let decodeString1 = someJson->Js_json.decodeString
let decodeString2 = Js_json.decodeString(someJson)
let decodeString3 =
  [1, 2, 3]
  ->Array.map(v => v->Int.toString)
  ->Array.join(" ")
  ->strToJson
  ->Js_json.decodeString
