let check_record_fields (fields1 : Types.label_declaration list)
    (fields2 : Types.label_declaration list) =
  let violation = ref false in
  let label_decl_sub (acc1, acc2) (ld2 : Types.label_declaration) =
    match
      Ext_list.find_first fields1 (fun ld1 -> ld1.ld_id.name = ld2.ld_id.name)
    with
    | Some ld1 ->
      if ld1.ld_optional <> ld2.ld_optional then
        (* optional field can't be modified *)
        violation := true;
      let get_as (({txt}, payload) : Parsetree.attribute) =
        if txt = "as" then Ast_payload.is_single_string payload else None
      in
      let get_as_name (ld : Types.label_declaration) =
        match Ext_list.filter_map ld.ld_attributes get_as with
        | [] -> ld.ld_id.name
        | (s, _) :: _ -> s
      in
      if get_as_name ld1 <> get_as_name ld2 then violation := true;
      (ld1.ld_type :: acc1, ld2.ld_type :: acc2)
    | None ->
      (* field must be present *)
      violation := true;
      (acc1, acc2)
  in
  let tl1, tl2 = List.fold_left label_decl_sub ([], []) fields2 in
  (!violation, tl1, tl2)
