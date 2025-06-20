type record_field_subtype_violation =
  | Optional_mismatch of {
      label: string;
      left_optional: bool;
      right_optional: bool;
    }
  | Field_runtime_name_mismatch of {
      label: string;
      left_as: string option;
      right_as: string option;
    }
  | Field_missing of {label: string}

let check_record_fields (fields1 : Types.label_declaration list)
    (fields2 : Types.label_declaration list) =
  let violations = ref [] in
  let add_violation v = violations := v :: !violations in
  let label_decl_sub (acc1, acc2) (ld2 : Types.label_declaration) =
    match
      Ext_list.find_first fields1 (fun ld1 -> ld1.ld_id.name = ld2.ld_id.name)
    with
    | Some ld1 ->
      if ld1.ld_optional <> ld2.ld_optional then
        (* optional field can't be modified *)
        add_violation
          (Optional_mismatch
             {
               label = ld1.ld_id.name;
               left_optional = ld1.ld_optional;
               right_optional = ld2.ld_optional;
             });
      let get_as (({txt}, payload) : Parsetree.attribute) =
        if txt = "as" then Ast_payload.is_single_string payload else None
      in
      let get_as_name (ld : Types.label_declaration) =
        match Ext_list.filter_map ld.ld_attributes get_as with
        | [] -> None
        | (s, _) :: _ -> Some s
      in
      let get_label_runtime_name (ld : Types.label_declaration) =
        match get_as_name ld with
        | None -> ld.ld_id.name
        | Some s -> s
      in
      if get_label_runtime_name ld1 <> get_label_runtime_name ld2 then
        add_violation
          (Field_runtime_name_mismatch
             {
               label = ld1.ld_id.name;
               left_as = get_as_name ld1;
               right_as = get_as_name ld2;
             });
      (ld1.ld_type :: acc1, ld2.ld_type :: acc2)
    | None ->
      (* field must be present *)
      add_violation (Field_missing {label = ld2.ld_id.name});
      (acc1, acc2)
  in
  let tl1, tl2 = List.fold_left label_decl_sub ([], []) fields2 in
  (!violations, tl1, tl2)
