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

let decodeNumber1 = someJson->Js_json.decodeNumber
let decodeNumber2 = Js_json.decodeNumber(someJson)

let decodeObject1 = someJson->Js_json.decodeObject
let decodeObject2 = Js_json.decodeObject(someJson)

let decodeArray1 = someJson->Js_json.decodeArray
let decodeArray2 = Js_json.decodeArray(someJson)

let decodeBoolean1 = someJson->Js_json.decodeBoolean
let decodeBoolean2 = Js_json.decodeBoolean(someJson)

let decodeNull1 = someJson->Js_json.decodeNull
let decodeNull2 = Js_json.decodeNull(someJson)
