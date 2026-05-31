let get key (t : Yojson.Safe.t) : Yojson.Safe.t option =
  match t with
  | `Assoc items -> List.assoc_opt key items
  | _ -> None

let to_list_opt json = try Some (Yojson.Safe.Util.to_list json) with _ -> None

let from_string_opt text =
  try Some (Yojson.Safe.from_string text) with _ -> None
