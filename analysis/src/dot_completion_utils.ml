let filter_record_fields ~env ~record_as_string ~prefix ~exact fields =
  fields
  |> Utils.filter_map (fun (field : Shared_types.field) ->
         if Utils.check_name field.fname.txt ~prefix ~exact then
           Some
             (Shared_types.Completion.create field.fname.txt ~env
                ?deprecated:field.deprecated ~docstring:field.docstring
                ~kind:(Shared_types.Completion.Field (field, record_as_string)))
         else None)

let field_completions_for_dot_completion ?pos_of_dot typ ~env ~state ~package
    ~prefix ~exact =
  let as_object = typ |> Type_utils.extract_object_type ~env ~state ~package in
  match as_object with
  | Some (obj_env, obj) ->
    (* Handle obj completion via dot *)
    if Debug.verbose () then
      Printf.printf "[dot_completion]--> Obj type found:\n";
    obj |> Type_utils.get_obj_fields
    |> Utils.filter_map (fun (field, _typ) ->
           if Utils.check_name field ~prefix ~exact then
             let full_obj_field_name = Printf.sprintf "[\"%s\"]" field in
             Some
               (Shared_types.Completion.create full_obj_field_name
                  ~synthetic:true ~insert_text:full_obj_field_name ~env:obj_env
                  ~kind:(Shared_types.Completion.ObjLabel typ)
                  ?additional_text_edits:
                    (match pos_of_dot with
                    | None -> None
                    | Some pos_of_dot ->
                      Some
                        (Type_utils.make_additional_text_edits_for_removing_dot
                           pos_of_dot)))
           else None)
  | None -> (
    match typ |> Type_utils.extract_record_type ~env ~state ~package with
    | Some (env, fields, typ_decl) ->
      fields
      |> filter_record_fields ~env ~prefix ~exact
           ~record_as_string:
             (typ_decl.item.decl |> Shared.decl_to_string typ_decl.name.txt)
    | None -> [])
