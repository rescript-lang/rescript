open GenTypeCommon

type t = CodeItem.translation

let empty = ({import_types = []; code_items = []; type_declarations = []} : t)

let get_import_type_unique_name
    ({type_name; as_type_name} : CodeItem.import_type) =
  type_name
  ^
  match as_type_name with
  | None -> ""
  | Some s -> "_as_" ^ s

let import_type_compare i1 i2 =
  compare (i1 |> get_import_type_unique_name) (i2 |> get_import_type_unique_name)

let combine (translations : t list) : t =
  ( translations
  |> List.map (fun {CodeItem.import_types; code_items; type_declarations} ->
         ((import_types, code_items), type_declarations))
  |> List.split
  |> fun (x, y) -> (x |> List.split, y) )
  |> fun ((import_types, code_items), type_declarations) ->
  {
    CodeItem.import_types = import_types |> List.concat;
    code_items = code_items |> List.concat;
    type_declarations = type_declarations |> List.concat;
  }

(** Applies type parameters to types (for all) *)
let abstract_the_type_parameters ~type_vars type_ =
  match type_ with
  | Function function_ -> Function {function_ with type_vars}
  | _ -> type_

let dep_to_import_type ~config ~output_file_relative ~resolver (dep : dep) =
  match dep with
  | _ when dep |> Dependencies.is_internal -> []
  | External name when name = "list" ->
    [
      {
        CodeItem.type_name = "list";
        as_type_name = None;
        import_path =
          ModuleName.rescript_pervasives
          |> ModuleResolver.import_path_for_reason_module_name ~config
               ~output_file_relative ~resolver;
      };
    ]
  | External _ -> []
  | Internal _ -> []
  | Dot _ ->
    let module_name = dep |> Dependencies.get_outer_module_name in
    let type_name =
      dep |> Dependencies.remove_external_outer_module |> dep_to_string
    in
    let as_type_name =
      match dep |> Dependencies.is_internal with
      | true -> None
      | false -> Some (dep |> dep_to_string)
    in
    let import_path =
      module_name
      |> ModuleResolver.import_path_for_reason_module_name ~config
           ~output_file_relative ~resolver
    in
    [{type_name; as_type_name; import_path}]

let translate_dependencies ~config ~output_file_relative ~resolver dependencies
    : CodeItem.import_type list =
  dependencies
  |> List.map (dep_to_import_type ~config ~output_file_relative ~resolver)
  |> List.concat

let translate_value ~attributes ~config ~doc_string ~output_file_relative
    ~resolver ~type_env ~type_expr
    ~(add_annotations_to_function : type_ -> type_) name : t =
  let name_as =
    match Annotation.get_gentype_as_renaming attributes with
    | Some s -> s
    | _ -> name
  in
  let type_expr_translation =
    type_expr
    |> TranslateTypeExprFromTypes.translate_type_expr_from_types ~config
         ~type_env
  in
  let type_vars = type_expr_translation.type_ |> TypeVars.free in
  let type_ =
    type_expr_translation.type_
    |> abstract_the_type_parameters ~type_vars
    |> add_annotations_to_function
  in
  let resolved_name_original =
    name |> TypeEnv.add_module_path ~type_env |> ResolvedName.to_string
  in
  let resolved_name = name_as |> TypeEnv.add_module_path ~type_env in
  let module_access_path =
    type_env |> TypeEnv.get_module_access_path ~name:resolved_name_original
  in
  let code_items =
    [
      CodeItem.ExportValue
        {
          doc_string;
          module_access_path;
          original_name = name;
          resolved_name;
          type_;
        };
    ]
  in
  {
    import_types =
      type_expr_translation.dependencies
      |> translate_dependencies ~config ~output_file_relative ~resolver;
    code_items;
    type_declarations = [];
  }

(**
 [@genType]
 [@module] external myBanner : ReasonReact.reactClass = "./MyBanner";
*)
let translate_primitive ~config ~output_file_relative ~resolver ~type_env
    (value_description : Typedtree.value_description) : t =
  if !Debug.translation then Log_.item "Translate Primitive\n";
  let value_name =
    match value_description.val_prim with
    | "" :: _ | [] -> value_description.val_id |> Ident.name
    | name_of_extern :: _ ->
      (* extern foo : someType = "abc"
         The first element of val_prim is "abc" *)
      name_of_extern
  in
  let type_expr_translation =
    value_description.val_desc
    |> TranslateCoreType.translate_core_type ~config ~type_env
  in
  let attribute_import, attribute_renaming =
    value_description.val_attributes |> Annotation.get_attribute_import_renaming
  in
  match (type_expr_translation.type_, attribute_import) with
  | _, Some import_string ->
    let as_path =
      match attribute_renaming with
      | Some as_path -> as_path
      | None -> value_name
    in
    let type_vars = type_expr_translation.type_ |> TypeVars.free in
    let type_ =
      type_expr_translation.type_ |> abstract_the_type_parameters ~type_vars
    in
    {
      import_types =
        type_expr_translation.dependencies
        |> translate_dependencies ~config ~output_file_relative ~resolver;
      code_items =
        [
          ImportValue
            {
              as_path;
              import_annotation = import_string |> Annotation.import_from_string;
              type_;
              value_name;
            };
        ];
      type_declarations = [];
    }
  | _ -> {import_types = []; code_items = []; type_declarations = []}

let add_type_declarations_from_module_equations ~type_env (translation : t) =
  let eqs = type_env |> TypeEnv.get_module_equations in
  let new_type_declarations =
    translation.type_declarations
    |> List.map (fun (type_declaration : CodeItem.type_declaration) ->
           let export_type =
             type_declaration.export_from_type_declaration.export_type
           in
           let equations =
             export_type.resolved_type_name |> ResolvedName.apply_equations ~eqs
           in
           equations
           |> List.map (fun (x, y) ->
                  let new_export_type =
                    {
                      export_type with
                      name_as = None;
                      type_ =
                        y |> ResolvedName.to_string
                        |> ident ~builtin:false
                             ~type_args:
                               (export_type.type_vars
                               |> List.map (fun s -> TypeVar s));
                      resolved_type_name = x;
                    }
                  in
                  {
                    CodeItem.export_from_type_declaration =
                      {
                        CodeItem.export_type = new_export_type;
                        annotation =
                          type_declaration.export_from_type_declaration
                            .annotation;
                      };
                    import_types = [];
                  }))
    |> List.concat
  in
  match new_type_declarations = [] with
  | true -> translation
  | false ->
    {
      translation with
      type_declarations = translation.type_declarations @ new_type_declarations;
    }
