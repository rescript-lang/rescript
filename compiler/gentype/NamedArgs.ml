open GenTypeCommon

let group labeled_types =
  let types =
    Ext_list.map labeled_types (fun (lbl, a_type) ->
        match lbl with
        | Nolabel -> {a_name = ""; a_type}
        | Label lbl -> {a_name = lbl; a_type}
        | OptLabel lbl -> {a_name = lbl; a_type = Option a_type})
  in
  match types with
  | [{a_type}] when a_type = unit_t ->
    [] (* treat a single argument of type unit as no argument *)
  | _ -> types
