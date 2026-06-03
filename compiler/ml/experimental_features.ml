type feature = LetUnwrap

let to_string (f : feature) : string =
  match f with
  | LetUnwrap -> "LetUnwrap"

let from_string (s : string) : feature option =
  match s with
  | "LetUnwrap" -> Some LetUnwrap
  | _ -> None

module Feature_set = Set.Make (struct
  type t = feature
  let compare = compare
end)

let enabled_features : Feature_set.t ref = ref Feature_set.empty
let enable_from_string (s : string) =
  match from_string s with
  | Some f -> enabled_features := Feature_set.add f !enabled_features
  | None -> ()

let reset () = enabled_features := Feature_set.empty

let is_enabled (f : feature) = Feature_set.mem f !enabled_features
