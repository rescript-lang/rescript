let filter_record_fields ~env ~record_as_string ~prefix ~exact fields =
  fields
  |> Utils.filter_map (fun (field : SharedTypes.field) ->
         if Utils.check_name field.fname.txt ~prefix ~exact then
           Some
             (SharedTypes.Completion.create field.fname.txt ~env
                ?deprecated:field.deprecated ~docstring:field.docstring
                ~kind:(SharedTypes.Completion.Field (field, record_as_string)))
         else None)

let field_completions_for_dot_completion ?pos_of_dot typ ~env ~package ~prefix ~exact
    =
  let as_object = typ |> TypeUtils.extract_object_type ~env ~package in
  match as_object with
  | Some (obj_env, obj) ->
    (* Handle obj completion via dot *)
    if Debug.verbose () then
      Printf.printf "[dot_completion]--> Obj type found:\n";
    obj |> TypeUtils.get_obj_fields
    |> Utils.filter_map (fun (field, _typ) ->
           if Utils.check_name field ~prefix ~exact then
             let full_obj_field_name = Printf.sprintf "[\"%s\"]" field in
             Some
               (SharedTypes.Completion.create full_obj_field_name ~synthetic:true
                  ~insert_text:full_obj_field_name ~env:obj_env
                  ~kind:(SharedTypes.Completion.ObjLabel typ)
                  ?additional_text_edits:
                    (match pos_of_dot with
                    | None -> None
                    | Some pos_of_dot ->
                      Some
                        (TypeUtils.make_additional_text_edits_for_removing_dot
                           pos_of_dot)))
           else None)
  | None -> (
    match typ |> TypeUtils.extract_record_type ~env ~package with
    | Some (env, fields, typ_decl) ->
      fields
      |> filter_record_fields ~env ~prefix ~exact
           ~record_as_string:
             (typ_decl.item.decl |> Shared.decl_to_string typ_decl.name.txt)
    | None -> [])
