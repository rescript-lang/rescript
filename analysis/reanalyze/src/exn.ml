type t = string

let compare = String.compare
let decode_error = "DecodeError"
let assert_failure = "Assert_failure"
let division_by_zero = "Division_by_zero"
let failure = "Failure"
let invalid_argument = "Invalid_argument"
let js_exn = "JsExn"
let match_failure = "Match_failure"
let not_found = "Not_found"
let from_lid lid = lid |> Longident.flatten |> String.concat "."
let from_string s = s
let to_string s = s
let yojson_json_error = "Yojson.Json_error"
let yojson_type_error = "Yojson.Basic.Util.Type_error"
