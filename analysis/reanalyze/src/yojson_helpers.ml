let get key (t : Yojson.Safe.t) : Yojson.Safe.t option =
  match t with
  | `Assoc items -> List.assoc_opt key items
  | _ -> None

let bool_opt = function
  | `Bool value -> Some value
  | _ -> None

let string_opt = function
  | `String value -> Some value
  | _ -> None

let to_list_opt = function
  | `List items -> Some items
  | _ -> None

let from_string_opt text =
  try Some (Yojson.Safe.from_string text) with _ -> None
