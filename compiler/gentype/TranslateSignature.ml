open GenTypeCommon

let translate_signature_value ~config ~output_file_relative ~resolver ~type_env
    (value_description : Typedtree.value_description) : Translation.t =
  let {Typedtree.val_attributes; val_desc; val_id; val_loc} =
    value_description
  in
  if !Debug.translation then
    Log_.item "Translate Signature Value %s\n" (val_id |> Ident.name);
  let type_expr = val_desc.ctyp_type in
  let add_annotations_to_function type_ = type_ in
  match
    (val_id, val_attributes |> Annotation.from_attributes ~config ~loc:val_loc)
  with
  | id, GenType ->
    id |> Ident.name
    |> Translation.translate_value ~attributes:val_attributes ~config
         ~doc_string:(Annotation.doc_string_from_attrs val_attributes)
         ~output_file_relative ~resolver ~type_env ~type_expr
         ~add_annotations_to_function
  | _ -> Translation.empty

let rec translate_module_declaration ~config ~output_file_relative ~resolver
    ~type_env ({md_id; md_type} : Typedtree.module_declaration) =
  let name = md_id |> Ident.name in
  if !Debug.translation then Log_.item "Translate Module Declaration %s\n" name;
  let type_env = type_env |> TypeEnv.new_module ~name in
  match md_type.mty_desc with
  | Tmty_signature signature ->
    signature
    |> translate_signature ~config ~output_file_relative ~resolver ~type_env
    |> Translation.combine
  | Tmty_ident (path, _) -> (
    match type_env |> TypeEnv.lookup_module_type_signature ~path with
    | None -> Translation.empty
    | Some (signature, _) ->
      signature
      |> translate_signature ~config ~output_file_relative ~resolver ~type_env
      |> Translation.combine)
  | Tmty_functor _ ->
    log_not_implemented ("Tmty_functor " ^ __LOC__);
    Translation.empty
  | Tmty_with _ ->
    log_not_implemented ("Tmty_with " ^ __LOC__);
    Translation.empty
  | Tmty_typeof _ ->
    log_not_implemented ("Tmty_typeof " ^ __LOC__);
    Translation.empty
  | Tmty_alias _ ->
    log_not_implemented ("Tmty_alias " ^ __LOC__);
    Translation.empty

and translate_module_type_declaration ~config ~output_file_relative ~resolver
    ~type_env (module_type_declaration : Typedtree.module_type_declaration) =
  if !Debug.translation then
    Log_.item "Translate Module Type Declaration %s\n"
      (module_type_declaration.mtd_id |> Ident.name);
  match module_type_declaration with
  | {mtd_type = None} -> Translation.empty
  | {mtd_id; mtd_type = Some mtd_type} -> (
    match mtd_type.mty_desc with
    | Tmty_signature signature ->
      let name = mtd_id |> Ident.name in
      (* Only translate types *)
      let signature_without_values =
        {
          signature with
          sig_items =
            Ext_list.filter signature.sig_items (function
              | {sig_desc = Tsig_value _} -> false
              | _ -> true);
        }
      in
      let translation =
        signature_without_values
        |> translate_signature ~config ~output_file_relative ~resolver
             ~type_env:(type_env |> TypeEnv.new_module_type ~name ~signature)
        |> Translation.combine
      in
      translation
    | Tmty_ident _ ->
      log_not_implemented ("Tmty_ident " ^ __LOC__);
      Translation.empty
    | Tmty_functor _ ->
      log_not_implemented ("Tmty_functor " ^ __LOC__);
      Translation.empty
    | Tmty_with _ ->
      log_not_implemented ("Tmty_with " ^ __LOC__);
      Translation.empty
    | Tmty_typeof _ ->
      log_not_implemented ("Tmty_typeof " ^ __LOC__);
      Translation.empty
    | Tmty_alias _ ->
      log_not_implemented ("Tmty_alias " ^ __LOC__);
      Translation.empty)

and translate_signature_item ~config ~output_file_relative ~resolver ~type_env
    signature_item : Translation.t =
  match signature_item with
  | {Typedtree.sig_desc = Typedtree.Tsig_type (rec_flag, type_declarations)} ->
    {
      import_types = [];
      code_items = [];
      type_declarations =
        type_declarations
        |> TranslateTypeDeclarations.translate_type_declarations ~config
             ~output_file_relative ~recursive:(rec_flag = Recursive) ~resolver
             ~type_env;
    }
  | {Typedtree.sig_desc = Tsig_value value_description} ->
    let is_import =
      value_description.val_attributes
      |> Annotation.has_attribute Annotation.tag_is_gentype_import
    in
    if value_description.val_prim <> [] || is_import then
      value_description
      |> Translation.translate_primitive ~config ~output_file_relative ~resolver
           ~type_env
    else
      let module_item =
        Runtime.new_module_item ~name:(value_description.val_id |> Ident.name)
      in
      type_env |> TypeEnv.update_module_item ~module_item;
      value_description
      |> translate_signature_value ~config ~output_file_relative ~resolver
           ~type_env
  | {Typedtree.sig_desc = Typedtree.Tsig_module module_declaration} ->
    module_declaration
    |> translate_module_declaration ~config ~output_file_relative ~resolver
         ~type_env
  | {Typedtree.sig_desc = Typedtree.Tsig_modtype module_type_declaration} ->
    let module_item =
      Runtime.new_module_item
        ~name:(module_type_declaration.mtd_id |> Ident.name)
    in
    let config =
      module_type_declaration.mtd_attributes
      |> Annotation.update_config_for_module ~config
    in
    type_env |> TypeEnv.update_module_item ~module_item;
    module_type_declaration
    |> translate_module_type_declaration ~config ~output_file_relative ~resolver
         ~type_env
  | {Typedtree.sig_desc = Typedtree.Tsig_typext _} ->
    log_not_implemented ("Tsig_typext " ^ __LOC__);
    Translation.empty
  | {Typedtree.sig_desc = Typedtree.Tsig_exception _} ->
    log_not_implemented ("Tsig_exception " ^ __LOC__);
    Translation.empty
  | {Typedtree.sig_desc = Typedtree.Tsig_recmodule _} ->
    log_not_implemented ("Tsig_recmodule " ^ __LOC__);
    Translation.empty
  | {Typedtree.sig_desc = Typedtree.Tsig_open _} ->
    log_not_implemented ("Tsig_open " ^ __LOC__);
    Translation.empty
  | {Typedtree.sig_desc = Typedtree.Tsig_include _} ->
    log_not_implemented ("Tsig_include " ^ __LOC__);
    Translation.empty
  | {Typedtree.sig_desc = Typedtree.Tsig_attribute _} ->
    log_not_implemented ("Tsig_attribute " ^ __LOC__);
    Translation.empty

and translate_signature ~config ~output_file_relative ~resolver ~type_env
    signature : Translation.t list =
  if !Debug.translation then Log_.item "Translate Signature\n";
  signature.Typedtree.sig_items
  |> List.map
       (translate_signature_item ~config ~output_file_relative ~resolver
          ~type_env)
