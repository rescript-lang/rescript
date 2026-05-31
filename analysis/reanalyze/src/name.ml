type t = string

let compare = String.compare

let create ?(is_interface = true) s =
  match is_interface with
  | true -> s
  | false -> "+" ^ s

let is_interface s = try s.[0] <> '+' with Invalid_argument _ -> false
let is_underscore s = s = "_" || s = "+_"

let starts_with_underscore s =
  s |> String.length >= 2
  &&
  try s.[0] = '_' || (s.[0] = '+' && s.[1] = '_')
  with Invalid_argument _ -> false

let to_interface s =
  match is_interface s with
  | true -> s
  | false -> (
    try String.sub s 1 (String.length s - 1) with Invalid_argument _ -> s)

let to_implementation s =
  match is_interface s with
  | true -> "+" ^ s
  | false -> s
let to_string (s : t) = s
