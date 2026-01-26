open GenTypeCommon

let rec addAnnotationsToTypes_ ~config ~(expr : Typedtree.expression)
    (arg_types : arg_type list) =
  match (expr.exp_desc, expr.exp_type.desc, arg_types) with
  | Texp_function {arg_label; param; case = {c_rhs}}, _, {a_type} :: next_types
    ->
    let next_types1 =
      next_types |> addAnnotationsToTypes_ ~config ~expr:c_rhs
    in
    let a_name = Ident.name param in
    let _ = Printtyped.implementation in
    let a_name =
      if a_name = "*opt*" then
        match arg_label with
        | Optional {txt = l} -> l
        | _ -> "" (* should not happen *)
      else a_name
    in
    {a_name; a_type} :: next_types1
  | ( Texp_apply
        {funct = {exp_desc = Texp_ident (path, _, _)}; args = [(_, Some expr1)]},
      _,
      _ ) -> (
    match path |> TranslateTypeExprFromTypes.path_to_list |> List.rev with
    | ["Js"; "Internal"; fn_mk]
      when (* Uncurried function definition uses Js.Internal.fn_mkX(...) *)
           String.length fn_mk >= 5
           && (String.sub fn_mk 0 5 [@doesNotRaise]) = "fn_mk" ->
      arg_types |> addAnnotationsToTypes_ ~config ~expr:expr1
    | _ -> arg_types)
  | _ -> arg_types

and add_annotations_to_types ~config ~(expr : Typedtree.expression)
    (arg_types : arg_type list) =
  let arg_types = addAnnotationsToTypes_ ~config ~expr arg_types in
  if
    arg_types
    |> List.filter (fun {a_name} -> a_name = "param")
    |> List.length > 1
  then
    (* Underscore "_" appears as "param", can occur more than once *)
    arg_types
    |> List.mapi (fun i {a_name; a_type} ->
           {a_name = a_name ^ "_" ^ string_of_int i; a_type})
  else arg_types

and add_annotations_to_fields ~config (expr : Typedtree.expression)
    (fields : fields) (arg_types : arg_type list) =
  match (expr.exp_desc, fields, arg_types) with
  | _, [], _ -> ([], arg_types |> add_annotations_to_types ~config ~expr)
  | Texp_function {case = {c_rhs}}, field :: next_fields, _ ->
    let next_fields1, types1 =
      add_annotations_to_fields ~config c_rhs next_fields arg_types
    in
    let name =
      TranslateTypeDeclarations.rename_record_field
        ~attributes:expr.exp_attributes ~name:field.name_js
    in
    ({field with name_js = name} :: next_fields1, types1)
  | _ -> (fields, arg_types)
[@@live]

(** Recover from expr the renaming annotations on named arguments. *)
let add_annotations_to_function_type ~config (expr : Typedtree.expression)
    (type_ : type_) =
  match type_ with
  | Function function_ ->
    let arg_types =
      function_.arg_types |> add_annotations_to_types ~config ~expr
    in
    Function {function_ with arg_types}
  | _ -> type_

let remove_value_binding_duplicates structure_items =
  let rec process_bindings (bindings : Typedtree.value_binding list) ~seen =
    match bindings with
    | ({vb_pat = {pat_desc = Tpat_var (id, _)}} as binding) :: other_bindings ->
      let name = Ident.name id in
      if !seen |> StringSet.mem name then
        other_bindings |> process_bindings ~seen
      else (
        seen := !seen |> StringSet.add name;
        binding :: (other_bindings |> process_bindings ~seen))
    | binding :: other_bindings ->
      binding :: (other_bindings |> process_bindings ~seen)
    | [] -> []
  in
  let rec process_items (items : Typedtree.structure_item list) ~acc ~seen =
    match items with
    | ({Typedtree.str_desc = Tstr_value (loc, value_bindings)} as item)
      :: other_items ->
      let bindings = value_bindings |> process_bindings ~seen in
      let item = {item with str_desc = Tstr_value (loc, bindings)} in
      other_items |> process_items ~acc:(item :: acc) ~seen
    | item :: other_items ->
      other_items |> process_items ~acc:(item :: acc) ~seen
    | [] -> acc
  in
  structure_items |> List.rev
  |> process_items ~acc:[] ~seen:(ref StringSet.empty)

let translate_value_binding ~config ~output_file_relative ~resolver ~type_env
    {Typedtree.vb_attributes; vb_expr; vb_pat} : Translation.t =
  match vb_pat.pat_desc with
  | Tpat_var (id, _) | Tpat_alias ({pat_desc = Tpat_any}, id, _) ->
    let name = id |> Ident.name in
    if !Debug.translation then Log_.item "Translate Value Binding %s\n" name;
    let module_item = Runtime.new_module_item ~name in
    type_env |> TypeEnv.update_module_item ~module_item;
    if
      vb_attributes
      |> Annotation.from_attributes ~config ~loc:vb_pat.pat_loc
      = GenType
    then
      id |> Ident.name
      |> Translation.translate_value ~attributes:vb_attributes ~config
           ~doc_string:(Annotation.doc_string_from_attrs vb_attributes)
           ~output_file_relative ~resolver ~type_env ~type_expr:vb_pat.pat_type
           ~add_annotations_to_function:
             (add_annotations_to_function_type ~config vb_expr)
    else Translation.empty
  | _ -> Translation.empty

let rec remove_duplicate_value_bindings
    (structure_items : Typedtree.structure_item list) =
  match structure_items with
  | ({Typedtree.str_desc = Tstr_value (loc, value_bindings)} as structure_item)
    :: rest ->
    let bound_in_rest, filtered_rest =
      rest |> remove_duplicate_value_bindings
    in
    let value_bindings_filtered =
      value_bindings
      |> List.filter (fun value_binding ->
             match value_binding with
             | {Typedtree.vb_pat = {pat_desc = Tpat_var (id, _)}} ->
               not (bound_in_rest |> StringSet.mem (id |> Ident.name))
             | _ -> true)
    in
    let bound =
      value_bindings
      |> List.fold_left
           (fun bound (value_binding : Typedtree.value_binding) ->
             match value_binding with
             | {vb_pat = {pat_desc = Tpat_var (id, _)}} ->
               bound |> StringSet.add (id |> Ident.name)
             | _ -> bound)
           bound_in_rest
    in
    ( bound,
      {structure_item with str_desc = Tstr_value (loc, value_bindings_filtered)}
      :: filtered_rest )
  | structure_item :: rest ->
    let bound_in_rest, filtered_rest =
      rest |> remove_duplicate_value_bindings
    in
    (bound_in_rest, structure_item :: filtered_rest)
  | [] -> (StringSet.empty, [])

let rec translate_module_binding ~(config : GenTypeConfig.t)
    ~output_file_relative ~resolver ~type_env
    ({mb_id; mb_expr; mb_attributes} : Typedtree.module_binding) : Translation.t
    =
  let name = mb_id |> Ident.name in
  if !Debug.translation then Log_.item "Translate Module Binding %s\n" name;
  let module_item = Runtime.new_module_item ~name in
  let config = mb_attributes |> Annotation.update_config_for_module ~config in
  type_env |> TypeEnv.update_module_item ~module_item;
  let type_env = type_env |> TypeEnv.new_module ~name in
  match mb_expr.mod_desc with
  | Tmod_ident (path, _) -> (
    let dep = path |> Dependencies.from_path ~config ~type_env in
    let internal = dep |> Dependencies.is_internal in
    type_env |> TypeEnv.add_module_equation ~dep ~internal;
    match Env.scrape_alias mb_expr.mod_env mb_expr.mod_type with
    | Mty_signature signature ->
      (* Treat module M = N as include N *)
      signature
      |> TranslateSignatureFromTypes.translate_signature_from_types ~config
           ~output_file_relative ~resolver ~type_env
      |> Translation.combine
    | Mty_alias _ | Mty_ident _ | Mty_functor _ -> Translation.empty)
  | Tmod_structure structure ->
    let is_let_private =
      mb_attributes |> Annotation.has_attribute Annotation.tag_is_intern_local
    in
    if is_let_private then Translation.empty
    else
      structure
      |> translate_structure ~config ~output_file_relative ~resolver ~type_env
      |> Translation.combine
  | Tmod_apply _ -> (
    (* Only look at the resulting type of the module *)
    match mb_expr.mod_type with
    | Mty_signature signature ->
      signature
      |> TranslateSignatureFromTypes.translate_signature_from_types ~config
           ~output_file_relative ~resolver ~type_env
      |> Translation.combine
    | Mty_ident _ ->
      log_not_implemented ("Mty_ident " ^ __LOC__);
      Translation.empty
    | Mty_functor _ ->
      log_not_implemented ("Mty_functor " ^ __LOC__);
      Translation.empty
    | Mty_alias _ ->
      log_not_implemented ("Mty_alias " ^ __LOC__);
      Translation.empty)
  | Tmod_unpack (_, module_type) -> (
    match module_type with
    | Mty_signature signature ->
      signature
      |> TranslateSignatureFromTypes.translate_signature_from_types ~config
           ~output_file_relative ~resolver ~type_env
      |> Translation.combine
    | Mty_ident path -> (
      match type_env |> TypeEnv.lookup_module_type_signature ~path with
      | None -> Translation.empty
      | Some (signature, _) ->
        signature
        |> TranslateSignature.translate_signature ~config ~output_file_relative
             ~resolver ~type_env
        |> Translation.combine)
    | Mty_functor _ ->
      log_not_implemented ("Mty_functor " ^ __LOC__);
      Translation.empty
    | Mty_alias _ ->
      log_not_implemented ("Mty_alias " ^ __LOC__);
      Translation.empty)
  | Tmod_functor _ ->
    log_not_implemented ("Tmod_functor " ^ __LOC__);
    Translation.empty
  | Tmod_constraint (_, Mty_ident path, Tmodtype_explicit _, Tcoerce_none) -> (
    match type_env |> TypeEnv.lookup_module_type_signature ~path with
    | None -> Translation.empty
    | Some (signature, _) ->
      signature
      |> TranslateSignature.translate_signature ~config ~output_file_relative
           ~resolver ~type_env
      |> Translation.combine)
  | Tmod_constraint
      (_, Mty_signature signature, Tmodtype_explicit _, Tcoerce_none) ->
    signature
    |> TranslateSignatureFromTypes.translate_signature_from_types ~config
         ~output_file_relative ~resolver ~type_env
    |> Translation.combine
  | Tmod_constraint
      ( {mod_desc = Tmod_structure structure},
        _,
        Tmodtype_implicit,
        Tcoerce_structure _ ) ->
    {
      structure with
      str_items = structure.str_items |> remove_duplicate_value_bindings |> snd;
    }
    |> translate_structure ~config ~output_file_relative ~resolver ~type_env
    |> Translation.combine
  | Tmod_constraint
      ( _,
        _,
        Tmodtype_explicit {mty_desc = Tmty_signature {sig_type = signature}},
        _ ) ->
    signature
    |> TranslateSignatureFromTypes.translate_signature_from_types ~config
         ~output_file_relative ~resolver ~type_env
    |> Translation.combine
  | Tmod_constraint _ ->
    log_not_implemented ("Tmod_constraint " ^ __LOC__);
    Translation.empty

and translate_structure_item ~config ~output_file_relative ~resolver ~type_env
    (struct_item : Typedtree.structure_item) : Translation.t =
  match struct_item with
  | {str_desc = Tstr_type (rec_flag, type_declarations)} ->
    {
      import_types = [];
      code_items = [];
      type_declarations =
        type_declarations
        |> TranslateTypeDeclarations.translate_type_declarations ~config
             ~output_file_relative ~recursive:(rec_flag = Recursive) ~resolver
             ~type_env;
    }
  | {str_desc = Tstr_value (_loc, value_bindings)} ->
    value_bindings
    |> List.map
         (translate_value_binding ~config ~output_file_relative ~resolver
            ~type_env)
    |> Translation.combine
  | {str_desc = Tstr_primitive value_description} ->
    (* external declaration *)
    value_description
    |> Translation.translate_primitive ~config ~output_file_relative ~resolver
         ~type_env
  | {str_desc = Tstr_module module_binding} ->
    module_binding
    |> translate_module_binding ~config ~output_file_relative ~resolver
         ~type_env
  | {str_desc = Tstr_modtype module_type_declaration} ->
    module_type_declaration
    |> TranslateSignature.translate_module_type_declaration ~config
         ~output_file_relative ~resolver ~type_env
  | {str_desc = Tstr_recmodule module_bindings} ->
    module_bindings
    |> List.map
         (translate_module_binding ~config ~output_file_relative ~resolver
            ~type_env)
    |> Translation.combine
  | {
   str_desc =
     (* ReScript's encoding of @module: include with constraint. *)
     Tstr_include
       {
         incl_mod =
           {
             mod_desc =
               Tmod_constraint
                 ( {
                     mod_desc =
                       Tmod_structure
                         {
                           str_items =
                             [({str_desc = Tstr_primitive _} as struct_item1)];
                         };
                   },
                   _,
                   _,
                   _ );
           };
         _;
       };
   _;
  } ->
    struct_item1
    |> translate_structure_item ~config ~output_file_relative ~resolver
         ~type_env
  | {str_desc = Tstr_include {incl_type = signature}} ->
    signature
    |> TranslateSignatureFromTypes.translate_signature_from_types ~config
         ~output_file_relative ~resolver ~type_env
    |> Translation.combine
  | {str_desc = Tstr_eval _} ->
    log_not_implemented ("Tstr_eval " ^ __LOC__);
    Translation.empty
  | {str_desc = Tstr_typext _} ->
    log_not_implemented ("Tstr_typext " ^ __LOC__);
    Translation.empty
  | {str_desc = Tstr_exception _} ->
    log_not_implemented ("Tstr_exception " ^ __LOC__);
    Translation.empty
  | {str_desc = Tstr_open _} ->
    log_not_implemented ("Tstr_open " ^ __LOC__);
    Translation.empty
  | {str_desc = Tstr_attribute _} ->
    log_not_implemented ("Tstr_attribute " ^ __LOC__);
    Translation.empty

and translate_structure ~config ~output_file_relative ~resolver ~type_env
    structure : Translation.t list =
  if !Debug.translation then Log_.item "Translate Structure\n";
  structure.Typedtree.str_items |> remove_value_binding_duplicates
  |> List.map (fun struct_item ->
         struct_item
         |> translate_structure_item ~config ~output_file_relative ~resolver
              ~type_env)
