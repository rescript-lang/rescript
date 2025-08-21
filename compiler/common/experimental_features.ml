type feature = LetUnwrap

let to_string (f : feature) : string =
  match f with
  | LetUnwrap -> "LetUnwrap"

let from_string (s : string) : feature option =
  match s with
  | "LetUnwrap" -> Some LetUnwrap
  | _ -> None

let enabled_features : feature list ref = ref []
let enable_from_string (s : string) =
  match from_string s with
  | Some f -> enabled_features := f :: !enabled_features
  | None -> ()

let is_enabled (f : feature) = List.mem f !enabled_features
