open Gentype_common
open! Translate_type_expr_from_types

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
    ~no_function_return_dependencies ~type_env (params : Typedtree.arg list)
    (ret : Typedtree.core_type) =
  let rev_arg_deps, rev_args =
    List.fold_left
      (fun (rev_arg_deps, rev_args) ({attrs; lbl; typ} : Typedtree.arg) ->
        match lbl with
        | Nolabel ->
          let {dependencies; type_} =
            typ |> translateCoreType_ ~config ~type_vars_gen ~type_env
          in
          ( List.rev_append dependencies rev_arg_deps,
            (Nolabel, type_) :: rev_args )
        | (Labelled {txt = lbl_txt} | Optional {txt = lbl_txt}) as label -> (
          let as_label =
            match attrs |> Annotation.get_gentype_as_renaming with
            | Some s -> s
            | None -> ""
          in
          match typ |> remove_option ~label with
          | None ->
            let {dependencies; type_ = type1} =
              typ |> translateCoreType_ ~config ~type_vars_gen ~type_env
            in
            ( List.rev_append dependencies rev_arg_deps,
              ( Label
                  (match as_label = "" with
                  | true -> lbl_txt
                  | false -> as_label),
                type1 )
              :: rev_args )
          | Some (lbl_txt, t1) ->
            let {dependencies; type_ = type1} =
              t1 |> translateCoreType_ ~config ~type_vars_gen ~type_env
            in
            ( List.rev_append dependencies rev_arg_deps,
              (OptLabel lbl_txt, type1) :: rev_args )))
      ([], []) params
  in
  let {dependencies; type_ = ret_type} =
    ret |> translateCoreType_ ~config ~type_vars_gen ~type_env
  in
  let all_deps =
    List.rev_append rev_arg_deps
      (match no_function_return_dependencies with
      | true -> []
      | false -> dependencies)
  in
  let labeled_convertable_types = rev_args |> List.rev in
  let arg_types = labeled_convertable_types |> Named_args.group in
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
      | Typedtree.OTtag ({txt = name}, attrs, t) ->
        let mutable_ =
          if
            List.exists
              (fun (({txt}, payload) : Parsetree.attribute) ->
                txt = "set" && payload = Parsetree.PStr [])
              attrs
          then Mutable
          else Immutable
        in
        ( name,
          mutable_,
          t |> translateCoreType_ ~config ~type_vars_gen ~type_env )
      | OTinherit t ->
        ( "Inherit",
          Immutable,
          t |> translateCoreType_ ~config ~type_vars_gen ~type_env )
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
    Translate_type_expr_from_types.translate_constr ~config ~params_translation
      ~path ~type_env
  | Ttyp_poly (_, t) ->
    t
    |> translateCoreType_ ~config ~type_vars_gen
         ~no_function_return_dependencies ~type_env
  | Ttyp_arrow (params, ret) ->
    translate_arrow_type ~config ~type_vars_gen ~no_function_return_dependencies
      ~type_env params ret
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
                  if is_number label then IntLabel label else StringLabel label
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
              payload |> translateCoreType_ ~config ~type_vars_gen ~type_env ))
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
    match type_env |> Type_env.lookup_module_type_signature ~path:pack_path with
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
      let type_env1 = type_env |> Type_env.add_type_equations ~type_equations in
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
  let type_vars_gen = Gen_ident.create_type_vars_gen () in
  let translation =
    core_type |> translateCoreType_ ~config ~type_vars_gen ~type_env
  in
  if !Debug.dependencies then
    translation.dependencies
    |> List.iter (fun dep ->
        Log_.item "Dependency: %s\n" (dep |> dep_to_string));
  translation
