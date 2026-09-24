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

(* Feature switches belong to the compiling domain, including nested requests. *)
let key = Domain.DLS.new_key (fun () -> ref Feature_set.empty)
let enabled_features () = Domain.DLS.get key

let with_fresh action =
  let previous = enabled_features () in
  Domain.DLS.set key (ref Feature_set.empty);
  Fun.protect action ~finally:(fun () -> Domain.DLS.set key previous)

let enable_from_string (s : string) =
  match from_string s with
  | Some f ->
    let enabled = enabled_features () in
    enabled := Feature_set.add f !enabled
  | None -> ()

let reset () = enabled_features () := Feature_set.empty

let is_enabled (f : feature) = Feature_set.mem f !(enabled_features ())
