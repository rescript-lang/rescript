open Gentype_common

let group labeled_types =
  let types =
    List.map
      (fun (lbl, a_type) ->
        match lbl with
        | Nolabel -> {a_name = ""; a_type}
        | Label lbl -> {a_name = lbl; a_type}
        | OptLabel lbl -> {a_name = lbl; a_type = Option a_type})
      labeled_types
  in
  match types with
  | [{a_type}] when a_type = unit_t ->
    [] (* treat a single argument of type unit as no argument *)
  | _ -> types
