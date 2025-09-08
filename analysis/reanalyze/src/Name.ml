type t = string

let compare = String.compare

let create ?(isInterface = true) s =
  match isInterface with
  | true -> s
  | false -> "+" ^ s

let isInterface s = try s.[0] <> '+' with Invalid_argument _ -> false
let isUnderscore s = s = "_" || s = "+_"

let startsWithUnderscore s =
  s |> String.length >= 2
  &&
  try s.[0] = '_' || (s.[0] = '+' && s.[1] = '_')
  with Invalid_argument _ -> false

let toInterface s =
  match isInterface s with
  | true -> s
  | false -> (
    try String.sub s 1 (String.length s - 1) with Invalid_argument _ -> s)

let toImplementation s =
  match isInterface s with
  | true -> "+" ^ s
  | false -> s
let toString (s : t) = s
