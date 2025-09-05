open GenTypeCommon

let rec fold f acc (t : type_) =
  let acc = f acc t in
  match t with
  | Ident _ | TypeVar _ -> acc
  | Array (t1, _) | Dict t1 | Option t1 | Null t1 | Nullable t1 | Promise t1 ->
    fold f acc t1
  | Tuple ts -> List.fold_left (fold f) acc ts
  | Object (_shape, fields) ->
    List.fold_left (fun acc {type_} -> fold f acc type_) acc fields
  | Function {arg_types; ret_type; _} ->
    let acc =
      List.fold_left (fun acc {a_type} -> fold f acc a_type) acc arg_types
    in
    fold f acc ret_type
  | Variant {inherits; payloads; _} ->
    let acc = List.fold_left (fold f) acc inherits in
    List.fold_left (fun acc {t} -> fold f acc t) acc payloads
