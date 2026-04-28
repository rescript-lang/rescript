open GenTypeCommon
open! TranslateTypeExprFromTypes

let remove_option ~(label : Asttypes.arg_label)
    (core_type : Typedtree.core_type) =
  match (core_type.ctyp_desc, label) with
  | Ttyp_constr (Path.Pident id, _, [t]), Optional {txt = lbl}
    when Ident.name id = "option" ->
    Some (lbl, t)
  | ( Ttyp_constr (Pdot (Path.Pident name_space, id, _), _, [t]),
      Optional {txt = lbl} )
    when (* This has a different representation in 4.03+  *)
         Ident.name name_space = "FB" && id = "option" ->
    Some (lbl, t)
  | _ -> None

type process_variant = {
  no_payloads: (string * Typedtree.attributes) list;
  payloads: (string * Typedtree.attributes * Typedtree.core_type) list;
  inherits: Typedtree.core_type list;
}

let process_variant row_fields =
  let rec loop ~no_payloads ~payloads ~inherits fields =
    match fields with
    | Typedtree.Ttag
        ({txt = label}, attributes, _, (* only variants with no payload *) [])
      :: other_fields ->
      other_fields
      |> loop
           ~no_payloads:((label, attributes) :: no_payloads)
           ~payloads ~inherits
    | Ttag ({txt = label}, attributes, _, [payload]) :: other_fields ->
      other_fields
      |> loop ~no_payloads
           ~payloads:((label, attributes, payload) :: payloads)
           ~inherits
    | Ttag (_, _, _, _ :: _ :: _) :: other_fields ->
      (* Unknown: skipping *)
      other_fields |> loop ~no_payloads ~payloads ~inherits
    | Tinherit t :: other_fields ->
      other_fields |> loop ~no_payloads ~payloads ~inherits:(t :: inherits)
    | [] ->
      {
        no_payloads = no_payloads |> List.rev;
        payloads = payloads |> List.rev;
        inherits = inherits |> List.rev;
      }
  in
  row_fields |> loop ~no_payloads:[] ~payloads:[] ~inherits:[]

let rec translate_arrow_type ~config ~type_vars_gen
    ~no_function_return_dependencies ~type_env ~rev_arg_deps ~rev_args
    (core_type : Typedtree.core_type) =
  match core_type.ctyp_desc with
  | Ttyp_arrow ({lbl = Nolabel; typ = core_type1}, core_type2, arity)
    when arity = None || rev_args = [] ->
    let {dependencies; type_} =
      core_type1 |> fun __x ->
      translateCoreType_ ~config ~type_vars_gen ~type_env __x
    in
    let next_rev_deps = List.rev_append dependencies rev_arg_deps in
    core_type2
    |> translate_arrow_type ~config ~type_vars_gen
         ~no_function_return_dependencies ~type_env ~rev_arg_deps:next_rev_deps
         ~rev_args:((Nolabel, type_) :: rev_args)
  | Ttyp_arrow
      ( {
          lbl = (Labelled {txt = lbl} | Optional {txt = lbl}) as label;
          typ = core_type1;
        },
        core_type2,
        arity )
    when arity = None || rev_args = [] -> (
    let as_label =
      match core_type.ctyp_attributes |> Annotation.get_gentype_as_renaming with
      | Some s -> s
      | None -> ""
    in
    match core_type1 |> remove_option ~label with
    | None ->
      let {dependencies; type_ = type1} =
        core_type1 |> translateCoreType_ ~config ~type_vars_gen ~type_env
      in
      let next_rev_deps = List.rev_append dependencies rev_arg_deps in
      core_type2
      |> translate_arrow_type ~config ~type_vars_gen
           ~no_function_return_dependencies ~type_env
           ~rev_arg_deps:next_rev_deps
           ~rev_args:
             (( Label
                  (match as_label = "" with
                  | true -> lbl
                  | false -> as_label),
                type1 )
             :: rev_args)
    | Some (lbl, t1) ->
      let {dependencies; type_ = type1} =
        t1 |> translateCoreType_ ~config ~type_vars_gen ~type_env
      in
      let next_rev_deps = List.rev_append dependencies rev_arg_deps in
      core_type2
      |> translate_arrow_type ~config ~type_vars_gen
           ~no_function_return_dependencies ~type_env
           ~rev_arg_deps:next_rev_deps
           ~rev_args:((OptLabel lbl, type1) :: rev_args))
  | _ ->
    let {dependencies; type_ = ret_type} =
      core_type |> translateCoreType_ ~config ~type_vars_gen ~type_env
    in
    let all_deps =
      List.rev_append rev_arg_deps
        (match no_function_return_dependencies with
        | true -> []
        | false -> dependencies)
    in
    let labeled_convertable_types = rev_args |> List.rev in
    let arg_types = labeled_convertable_types |> NamedArgs.group in
    let function_type = Function {arg_types; ret_type; type_vars = []} in
    {dependencies = all_deps; type_ = function_type}

and translateCoreType_ ~config ~type_vars_gen
    ?(no_function_return_dependencies = false) ~type_env
    (core_type : Typedtree.core_type) =
  match core_type.ctyp_desc with
  | Ttyp_alias (ct, _) ->
    ct
    |> translateCoreType_ ~config ~type_vars_gen
         ~no_function_return_dependencies:false ~type_env
  | Ttyp_object (t_obj, closed_flag) ->
    let get_field_type object_field =
      match object_field with
      | Typedtree.OTtag ({txt = name}, _, t) ->
        ( name,
          match name |> Runtime.is_mutable_object_field with
          | true -> {dependencies = []; type_ = ident ""}
          | false -> t |> translateCoreType_ ~config ~type_vars_gen ~type_env )
      | OTinherit t ->
        ("Inherit", t |> translateCoreType_ ~config ~type_vars_gen ~type_env)
    in
    let fields_translations = t_obj |> List.map get_field_type in
    translate_obj_type
      (match closed_flag = Closed with
      | true -> Closed
      | false -> Open)
      fields_translations
  | Ttyp_constr (path, _, type_params) ->
    let params_translation =
      type_params |> translateCoreTypes_ ~config ~type_vars_gen ~type_env
    in
    TranslateTypeExprFromTypes.translate_constr ~config ~params_translation
      ~path ~type_env
  | Ttyp_poly (_, t) ->
    t
    |> translateCoreType_ ~config ~type_vars_gen
         ~no_function_return_dependencies ~type_env
  | Ttyp_arrow _ ->
    core_type
    |> translate_arrow_type ~config ~type_vars_gen
         ~no_function_return_dependencies ~type_env ~rev_arg_deps:[]
         ~rev_args:[]
  | Ttyp_tuple list_exp ->
    let inner_types_translation =
      list_exp |> translateCoreTypes_ ~config ~type_vars_gen ~type_env
    in
    let inner_types =
      inner_types_translation |> List.map (fun {type_} -> type_)
    in
    let inner_types_deps =
      inner_types_translation
      |> List.map (fun {dependencies} -> dependencies)
      |> List.concat
    in
    let tuple_type = Tuple inner_types in
    {dependencies = inner_types_deps; type_ = tuple_type}
  | Ttyp_var s -> {dependencies = []; type_ = TypeVar s}
  | Ttyp_variant (row_fields, _, _) -> (
    match row_fields |> process_variant with
    | {no_payloads; payloads; inherits} ->
      let as_string =
        core_type.ctyp_attributes
        |> Annotation.has_attribute Annotation.tag_is_string
      in
      let as_int =
        core_type.ctyp_attributes
        |> Annotation.has_attribute Annotation.tag_is_int
      in
      let last_bs_int = ref (-1) in
      let no_payloads =
        no_payloads
        |> List.map (fun (label, attributes) ->
               let label_js =
                 if as_string then
                   match attributes |> Annotation.get_as_string with
                   | Some label_renamed -> StringLabel label_renamed
                   | None ->
                     if is_number label then IntLabel label
                     else StringLabel label
                 else if as_int then (
                   match attributes |> Annotation.get_as_int with
                   | Some n ->
                     last_bs_int := n;
                     IntLabel (string_of_int n)
                   | None ->
                     last_bs_int := !last_bs_int + 1;
                     IntLabel (string_of_int !last_bs_int))
                 else if is_number label then IntLabel label
                 else StringLabel label
               in
               {label_js})
      in
      let payloads_translations =
        payloads
        |> List.map (fun (label, attributes, payload) ->
               ( label,
                 attributes,
                 payload |> translateCoreType_ ~config ~type_vars_gen ~type_env
               ))
      in
      let payloads =
        payloads_translations
        |> List.map (fun (label, _attributes, translation) ->
               {
                 case =
                   {
                     label_js =
                       (if is_number label then IntLabel label
                        else StringLabel label);
                   };
                 t = translation.type_;
               })
      in
      let inherits_translations =
        inherits |> translateCoreTypes_ ~config ~type_vars_gen ~type_env
      in
      let inherits = inherits_translations |> List.map (fun {type_} -> type_) in
      let type_ =
        create_variant ~no_payloads ~payloads ~inherits ~polymorphic:true
          ~tag:None ~unboxed:false
      in
      let dependencies =
        (inherits_translations
        |> List.map (fun {dependencies} -> dependencies)
        |> List.concat)
        @ (payloads_translations
          |> List.map (fun (_, _, {dependencies}) -> dependencies)
          |> List.concat)
      in
      {dependencies; type_})
  | Ttyp_package {pack_path; pack_fields} -> (
    match type_env |> TypeEnv.lookup_module_type_signature ~path:pack_path with
    | Some (signature, type_env) ->
      let type_equations_translation =
        pack_fields
        |> List.map (fun (x, t) ->
               ( x.Asttypes.txt,
                 t |> translateCoreType_ ~config ~type_vars_gen ~type_env ))
      in
      let type_equations =
        type_equations_translation
        |> List.map (fun (x, translation) -> (x, translation.type_))
      in
      let dependencies_from_type_equations =
        type_equations_translation
        |> List.map (fun (_, translation) -> translation.dependencies)
        |> List.flatten
      in
      let type_env1 = type_env |> TypeEnv.add_type_equations ~type_equations in
      let dependencies_from_record_type, type_ =
        signature.sig_type
        |> signature_to_module_runtime_representation ~config ~type_vars_gen
             ~type_env:type_env1
      in
      {
        dependencies =
          dependencies_from_type_equations @ dependencies_from_record_type;
        type_;
      }
    | None -> {dependencies = []; type_ = unknown})
  | Ttyp_any -> {dependencies = []; type_ = unknown}

and translateCoreTypes_ ~config ~type_vars_gen ~type_env type_exprs :
    translation list =
  type_exprs |> List.map (translateCoreType_ ~config ~type_vars_gen ~type_env)

let translate_core_type ~config ~type_env core_type =
  let type_vars_gen = GenIdent.create_type_vars_gen () in
  let translation =
    core_type |> translateCoreType_ ~config ~type_vars_gen ~type_env
  in
  if !Debug.dependencies then
    translation.dependencies
    |> List.iter (fun dep ->
           Log_.item "Dependency: %s\n" (dep |> dep_to_string));
  translation
