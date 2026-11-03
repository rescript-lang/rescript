type t = string

let compare = String.compare
let match_failure = "Match_failure"
let from_lid lid = lid |> Longident.flatten |> String.concat "."
let from_string s = s
let to_string s = s
