type feature = LetUnwrap

let to_string (f : feature) : string =
  match f with
  | LetUnwrap -> "LetUnwrap"

let from_string (s : string) : feature option =
  match s with
  | "LetUnwrap" -> Some LetUnwrap
  | _ -> None

module FeatureSet = Set.Make (struct
  type t = feature
  let compare = compare
end)

let enabled_features : FeatureSet.t ref = ref FeatureSet.empty
let enable_from_string (s : string) =
  match from_string s with
  | Some f -> enabled_features := FeatureSet.add f !enabled_features
  | None -> ()

let is_enabled (f : feature) = FeatureSet.mem f !enabled_features
