open GenTypeCommon

(** Like translateTypeDeclaration but from Types not Typedtree  *)
let translate_type_declaration_from_types ~config ~output_file_relative
    ~resolver ~type_env ~id
    ({type_attributes; type_kind; type_loc; type_manifest; type_params} :
      Types.type_declaration) : CodeItem.type_declaration list =
  type_env |> TypeEnv.new_type ~name:(id |> Ident.name);
  let type_name = Ident.name id in
  let type_vars = type_params |> TypeVars.extract_from_type_expr in
  if !Debug.translation then
    Log_.item "Translate Types.type_declaration %s\n" type_name;
  let declaration_kind =
    match type_kind with
    | Type_record (label_declarations, _) ->
      TranslateTypeDeclarations.RecordDeclarationFromTypes label_declarations
    | Type_variant constructor_declarations
      when not
             (TranslateTypeDeclarations.has_some_gadt_leaf
                constructor_declarations) ->
      VariantDeclarationFromTypes constructor_declarations
    | Type_abstract -> GeneralDeclarationFromTypes type_manifest
    | _ -> NoDeclaration
  in
  declaration_kind
  |> TranslateTypeDeclarations.traslate_declaration_kind ~config ~loc:type_loc
       ~output_file_relative ~resolver ~type_attributes ~type_env ~type_name
       ~type_vars

(** Like translateModuleDeclaration but from Types not Typedtree *)
let rec translate_module_declaration_from_types ~config ~output_file_relative
    ~resolver ~type_env ~id (module_declaration : Types.module_declaration) :
    Translation.t =
  match module_declaration.md_type with
  | Mty_signature signature ->
    let name = id |> Ident.name in
    signature
    |> translate_signature_from_types ~config ~output_file_relative ~resolver
         ~type_env:(type_env |> TypeEnv.new_module ~name)
    |> Translation.combine
  | Mty_ident _ ->
    log_not_implemented ("Mty_ident " ^ __LOC__);
    Translation.empty
  | Mty_functor _ ->
    log_not_implemented ("Mty_functor " ^ __LOC__);
    Translation.empty
  | Mty_alias _ ->
    log_not_implemented ("Mty_alias " ^ __LOC__);
    Translation.empty

(** Like translateSignatureItem but from Types not Typedtree  *)
and translate_signature_item_from_types ~config ~output_file_relative ~resolver
    ~type_env (signature_item : Types.signature_item) : Translation.t =
  match signature_item with
  | Types.Sig_type (id, type_declaration, _) ->
    {
      import_types = [];
      code_items = [];
      type_declarations =
        type_declaration
        |> translate_type_declaration_from_types ~config ~output_file_relative
             ~resolver ~type_env ~id;
    }
  | Types.Sig_module (id, module_declaration, _) ->
    let module_item = Runtime.new_module_item ~name:(id |> Ident.name) in
    let config =
      module_declaration.md_attributes
      |> Annotation.update_config_for_module ~config
    in
    type_env |> TypeEnv.update_module_item ~module_item;
    module_declaration
    |> translate_module_declaration_from_types ~config ~output_file_relative
         ~resolver ~type_env ~id
  | Types.Sig_value (id, {val_attributes; val_loc; val_type}) ->
    let name = id |> Ident.name in
    if !Debug.translation then Log_.item "Translate Sig Value %s\n" name;
    let module_item = Runtime.new_module_item ~name in
    type_env |> TypeEnv.update_module_item ~module_item;
    if
      val_attributes
      |> Annotation.from_attributes ~config ~loc:val_loc
      = GenType
    then
      name
      |> Translation.translate_value ~attributes:val_attributes ~config
           ~doc_string:(Annotation.doc_string_from_attrs val_attributes)
           ~output_file_relative ~resolver ~type_env ~type_expr:val_type
           ~add_annotations_to_function:(fun t -> t)
    else Translation.empty
  | Types.Sig_typext _ ->
    log_not_implemented ("Sig_typext " ^ __LOC__);
    Translation.empty
  | Types.Sig_modtype _ ->
    log_not_implemented ("Sig_modtype " ^ __LOC__);
    Translation.empty
  | Types.Sig_class _ ->
    log_not_implemented ("Sig_class " ^ __LOC__);
    Translation.empty
  | Types.Sig_class_type _ ->
    log_not_implemented ("Sig_class_type " ^ __LOC__);
    Translation.empty

(** Like translateSignature but from Types not Typedtree *)
and translate_signature_from_types ~config ~output_file_relative ~resolver
    ~type_env (signature : Types.signature_item list) : Translation.t list =
  if !Debug.translation then Log_.item "Translate Types.signature\n";
  signature
  |> List.map
       (translate_signature_item_from_types ~config ~output_file_relative
          ~resolver ~type_env)
