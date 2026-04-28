open GenTypeCommon

type declaration_kind =
  | RecordDeclarationFromTypes of Types.label_declaration list
  | GeneralDeclaration of Typedtree.core_type option
  | GeneralDeclarationFromTypes of Types.type_expr option
      (** As the above, but from Types not Typedtree *)
  | VariantDeclarationFromTypes of Types.constructor_declaration list
  | NoDeclaration

let create_export_type_from_type_declaration ~annotation ~loc ~name_as ~opaque
    ~type_ ~type_env ~doc_string type_name ~type_vars :
    CodeItem.export_from_type_declaration =
  let resolved_type_name =
    type_name |> sanitize_type_name |> TypeEnv.add_module_path ~type_env
  in
  {
    export_type =
      {loc; name_as; opaque; type_; type_vars; resolved_type_name; doc_string};
    annotation;
  }

let create_case (label, attributes) ~poly =
  {
    label_js =
      (match
         attributes |> Annotation.get_attribute_payload Annotation.tag_is_as
       with
      | Some (_, IdentPayload (Lident "null")) -> NullLabel
      | Some (_, IdentPayload (Lident "undefined")) -> UndefinedLabel
      | Some (_, BoolPayload b) -> BoolLabel b
      | Some (_, FloatPayload s) -> FloatLabel s
      | Some (_, IntPayload i) -> IntLabel i
      | Some (_, StringPayload as_label) -> StringLabel as_label
      | _ ->
        if poly && is_number label then IntLabel label else StringLabel label);
  }

(**
 * Rename record fields.
 * If @genType.as is used, perform renaming conversion.
 * If @as is used (with records-as-objects active), escape and quote if
 * the identifier contains characters which are invalid as JS property names.
 * For escaped identifiers like \"foo-bar", strip the surrounding \"..."
 * since they are part of the ReScript syntax, not the actual field name.
 * The resulting name will be quoted later in EmitType if needed.
*)
let rename_record_field ~attributes ~name =
  attributes |> Annotation.check_unsupported_gentype_as_renaming;
  match attributes |> Annotation.get_as_string with
  | Some s -> s |> String.escaped
  | None -> name |> Ext_ident.unwrap_uppercase_exotic

let traslate_declaration_kind ~config ~loc ~output_file_relative ~resolver
    ~type_attributes ~type_env ~type_name ~type_vars declaration_kind :
    CodeItem.type_declaration list =
  let doc_string = type_attributes |> Annotation.doc_string_from_attrs in
  let annotation = type_attributes |> Annotation.from_attributes ~config ~loc in
  let opaque =
    match annotation = Annotation.GenTypeOpaque with
    | true -> Some true
    | false -> None
    (* one means don't know *)
  in
  let import_string_opt, name_as =
    type_attributes |> Annotation.get_attribute_import_renaming
  in
  let unboxed_annotation =
    type_attributes |> Annotation.has_attribute Annotation.tag_is_unboxed
  in
  let tag_annotation = type_attributes |> Annotation.get_tag in
  let return_type_declaration (type_declaration : CodeItem.type_declaration) =
    match opaque = Some true with
    | true -> [{type_declaration with import_types = []}]
    | false -> [type_declaration]
  in
  let handle_general_declaration
      (translation : TranslateTypeExprFromTypes.translation) =
    let export_from_type_declaration =
      type_name
      |> create_export_type_from_type_declaration ~annotation ~loc ~name_as
           ~opaque ~type_:translation.type_ ~type_env ~doc_string ~type_vars
    in
    let import_types =
      translation.dependencies
      |> Translation.translate_dependencies ~config ~output_file_relative
           ~resolver
    in
    {CodeItem.import_types; export_from_type_declaration}
  in
  let translate_label_declarations ?(inline = false) label_declarations =
    let field_translations =
      label_declarations
      |> List.map
           (fun
             {Types.ld_id; ld_mutable; ld_optional; ld_type; ld_attributes} ->
             let name =
               rename_record_field ~attributes:ld_attributes
                 ~name:(ld_id |> Ident.name)
             in
             let mutability =
               match ld_mutable = Mutable with
               | true -> Mutable
               | false -> Immutable
             in
             ( name,
               mutability,
               ld_optional,
               ld_type
               |> TranslateTypeExprFromTypes.translate_type_expr_from_types
                    ~config ~type_env,
               Annotation.doc_string_from_attrs ld_attributes ))
    in
    let dependencies =
      field_translations
      |> List.map
           (fun (_, _, _, {TranslateTypeExprFromTypes.dependencies}, _) ->
             dependencies)
      |> List.concat
    in
    let fields =
      field_translations
      |> List.map
           (fun
             ( name,
               mutable_,
               optional_,
               {TranslateTypeExprFromTypes.type_},
               doc_string )
           ->
             let optional, type1 =
               match type_ with
               | Option type1 when optional_ -> (Optional, type1)
               | _ -> (Mandatory, type_)
             in
             {mutable_; name_js = name; optional; type_ = type1; doc_string})
    in
    let type_ =
      match fields with
      | [field] when unboxed_annotation -> field.type_
      | _ -> Object ((if inline then Inline else Closed), fields)
    in
    {TranslateTypeExprFromTypes.dependencies; type_}
  in
  match (declaration_kind, import_string_opt) with
  | _, Some import_string ->
    (* import type *)
    let typeName_ = type_name in
    let name_with_module_path =
      typeName_ |> TypeEnv.add_module_path ~type_env |> ResolvedName.to_string
    in
    let type_name, as_type_name =
      match name_as with
      | Some as_string -> (as_string, "$$" ^ name_with_module_path)
      | None -> (name_with_module_path, "$$" ^ name_with_module_path)
    in
    let import_types =
      [
        {
          CodeItem.type_name;
          as_type_name = Some as_type_name;
          import_path = import_string |> ImportPath.from_string_unsafe;
        };
      ]
    in
    let export_from_type_declaration =
      (* Make the imported type usable from other modules by exporting it too. *)
      typeName_
      |> create_export_type_from_type_declaration ~doc_string
           ~annotation:GenType ~loc ~name_as:None ~opaque:(Some false)
           ~type_:
             (as_type_name
             |> ident ~type_args:(type_vars |> List.map (fun s -> TypeVar s)))
           ~type_env ~type_vars
    in
    [{CodeItem.import_types; export_from_type_declaration}]
  | (GeneralDeclarationFromTypes None | GeneralDeclaration None), None ->
    {
      CodeItem.import_types = [];
      export_from_type_declaration =
        type_name
        |> create_export_type_from_type_declaration ~doc_string ~annotation ~loc
             ~name_as ~opaque:(Some true) ~type_:unknown ~type_env ~type_vars;
    }
    |> return_type_declaration
  | GeneralDeclarationFromTypes (Some type_expr), None ->
    let translation =
      type_expr
      |> TranslateTypeExprFromTypes.translate_type_expr_from_types ~config
           ~type_env
    in
    translation |> handle_general_declaration |> return_type_declaration
  | GeneralDeclaration (Some core_type), None ->
    let translation =
      core_type |> TranslateCoreType.translate_core_type ~config ~type_env
    in
    let type_ =
      match (core_type, translation.type_) with
      | {ctyp_desc = Ttyp_variant (row_fields, _, _)}, Variant variant ->
        let row_fields_variants =
          row_fields |> TranslateCoreType.process_variant
        in
        let no_payloads =
          row_fields_variants.no_payloads |> List.map (create_case ~poly:true)
        in
        let payloads =
          if
            variant.payloads |> List.length
            = (row_fields_variants.payloads |> List.length)
          then
            (List.combine variant.payloads row_fields_variants.payloads
             [@doesNotRaise])
            |> List.map (fun (payload, (label, attributes, _)) ->
                   let case = (label, attributes) |> create_case ~poly:true in
                   {payload with case})
          else variant.payloads
        in
        create_variant ~inherits:variant.inherits ~no_payloads ~payloads
          ~polymorphic:true ~tag:None ~unboxed:false
      | _ -> translation.type_
    in
    {translation with type_} |> handle_general_declaration
    |> return_type_declaration
  | RecordDeclarationFromTypes label_declarations, None ->
    let {TranslateTypeExprFromTypes.dependencies; type_} =
      label_declarations |> translate_label_declarations
    in
    let import_types =
      dependencies
      |> Translation.translate_dependencies ~config ~output_file_relative
           ~resolver
    in
    {
      CodeItem.import_types;
      export_from_type_declaration =
        type_name
        |> create_export_type_from_type_declaration ~doc_string ~annotation ~loc
             ~name_as ~opaque ~type_ ~type_env ~type_vars;
    }
    |> return_type_declaration
  | VariantDeclarationFromTypes constructor_declarations, None ->
    let variants =
      constructor_declarations
      |> List.map (fun constructor_declaration ->
             let constructor_args = constructor_declaration.Types.cd_args in
             let attributes = constructor_declaration.cd_attributes in
             let name = constructor_declaration.cd_id |> Ident.name in
             let args_translation =
               match constructor_args with
               | Cstr_tuple type_exprs ->
                 type_exprs
                 |> TranslateTypeExprFromTypes.translate_type_exprs_from_types
                      ~config ~type_env
               | Cstr_record label_declarations ->
                 [
                   label_declarations
                   |> translate_label_declarations ~inline:true;
                 ]
             in
             let arg_types =
               args_translation
               |> List.map (fun {TranslateTypeExprFromTypes.type_} -> type_)
             in
             let import_types =
               args_translation
               |> List.map (fun {TranslateTypeExprFromTypes.dependencies} ->
                      dependencies)
               |> List.concat
               |> Translation.translate_dependencies ~config
                    ~output_file_relative ~resolver
             in
             (name, attributes, arg_types, import_types))
    in
    let variants_no_payload, variants_with_payload =
      variants |> List.partition (fun (_, _, arg_types, _) -> arg_types = [])
    in
    let no_payloads =
      variants_no_payload
      |> List.map (fun (name, attributes, _argTypes, _importTypes) ->
             (name, attributes) |> create_case ~poly:false)
    in
    let payloads =
      variants_with_payload
      |> List.map (fun (name, attributes, arg_types, _importTypes) ->
             let type_ =
               match arg_types with
               | [type_] -> type_
               | _ -> Tuple arg_types
             in
             {case = (name, attributes) |> create_case ~poly:false; t = type_})
    in
    let variant_typ =
      create_variant ~inherits:[] ~no_payloads ~payloads ~polymorphic:false
        ~tag:tag_annotation ~unboxed:unboxed_annotation
    in
    let resolved_type_name =
      type_name |> sanitize_type_name |> TypeEnv.add_module_path ~type_env
    in
    let export_from_type_declaration =
      {
        CodeItem.export_type =
          {
            loc;
            name_as;
            opaque;
            type_ = variant_typ;
            type_vars;
            resolved_type_name;
            doc_string;
          };
        annotation;
      }
    in
    let import_types =
      variants
      |> List.map (fun (_, _, _, import_types) -> import_types)
      |> List.concat
    in
    {CodeItem.export_from_type_declaration; import_types}
    |> return_type_declaration
  | NoDeclaration, None -> []

let has_some_gadt_leaf constructor_declarations =
  List.exists
    (fun declaration -> declaration.Types.cd_res != None)
    constructor_declarations

let translate_type_declaration ~config ~output_file_relative ~resolver ~type_env
    ({typ_attributes; typ_id; typ_loc; typ_manifest; typ_params; typ_type} :
      Typedtree.type_declaration) : CodeItem.type_declaration list =
  if !Debug.translation then
    Log_.item "Translate Type Declaration %s\n" (typ_id |> Ident.name);

  let type_name = Ident.name typ_id in
  let type_vars =
    typ_params
    |> List.map (fun (core_type, _) -> core_type)
    |> TypeVars.extract_from_core_type
  in
  let declaration_kind =
    match typ_type.type_kind with
    | Type_record (label_declarations, _) ->
      RecordDeclarationFromTypes label_declarations
    | Type_variant constructor_declarations ->
      VariantDeclarationFromTypes constructor_declarations
    | Type_abstract -> GeneralDeclaration typ_manifest
    | _ -> NoDeclaration
  in
  declaration_kind
  |> traslate_declaration_kind ~config ~loc:typ_loc ~output_file_relative
       ~resolver ~type_attributes:typ_attributes ~type_env ~type_name ~type_vars

let add_type_declaration_id_to_type_env ~type_env
    ({typ_id} : Typedtree.type_declaration) =
  type_env |> TypeEnv.new_type ~name:(typ_id |> Ident.name)

let translate_type_declarations ~config ~output_file_relative ~recursive
    ~resolver ~type_env (type_declarations : Typedtree.type_declaration list) :
    CodeItem.type_declaration list =
  if recursive then
    type_declarations
    |> List.iter (add_type_declaration_id_to_type_env ~type_env);
  type_declarations
  |> List.map (fun type_declaration ->
         let res =
           type_declaration
           |> translate_type_declaration ~config ~output_file_relative ~resolver
                ~type_env
         in
         if not recursive then
           type_declaration |> add_type_declaration_id_to_type_env ~type_env;
         res)
  |> List.concat
