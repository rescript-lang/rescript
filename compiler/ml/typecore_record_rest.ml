open Types
open Format

type error =
  | Invalid_type
  | Requires_type_annotation of string
  | Not_record of Longident.t
  | Field_not_optional of string list * Longident.t
  | Field_missing of string list * Longident.t
  | Extra_field of string * Longident.t
  | Field_runtime_name_mismatch of {
      field: string;
      rest_type: Longident.t;
      source_runtime_name: string;
      rest_runtime_name: string;
    }

exception Error of Location.t * Env.t * error

type source_field = {
  source_name: string;
  source_runtime_name: string;
  source_type: type_expr;
}

let raise_error loc env err = raise (Error (loc, env, err))

let runtime_label_name name attrs =
  Ext_list.find_def attrs Lambda.find_name name

let runtime_label_description_name (lbl : label_description) =
  runtime_label_name lbl.lbl_name lbl.lbl_attributes

let runtime_label_declaration_name (lbl : label_declaration) =
  runtime_label_name (Ident.name lbl.ld_id) lbl.ld_attributes

let extract_instantiated_concrete_typedecl ~unify_pat_types env loc ty =
  let _, _, decl = Ctype.extract_concrete_typedecl env ty in
  let decl = Ctype.instance_declaration decl in
  let args =
    match Ctype.expand_head env ty with
    | {desc = Tconstr (_, args, _)} -> args
    | _ -> assert false
  in
  List.iter2
    (fun param arg -> unify_pat_types loc env param arg)
    decl.type_params args;
  decl

let type_args_from_annotation ~env ~pattern_force
    ~(rest_type_lid : Longident.t Location.loc) rest_decl rest_type_args_syntax
    =
  match rest_type_args_syntax with
  | [] -> List.map (fun _ -> Ctype.newvar ()) rest_decl.type_params
  | args ->
    let n_args = List.length args in
    let n_params = List.length rest_decl.type_params in
    if n_args <> n_params then
      raise
        (Typetexp.Error
           ( rest_type_lid.loc,
             env,
             Typetexp.Type_arity_mismatch (rest_type_lid.txt, n_params, n_args)
           ));
    List.map
      (fun sty ->
        let cty, force = Typetexp.transl_simple_type_delayed env sty in
        pattern_force := force :: !pattern_force;
        cty.ctyp_type)
      args

let source_fields_of_decl (fields : label_declaration list) =
  List.map
    (fun (field : label_declaration) ->
      {
        source_name = Ident.name field.ld_id;
        source_runtime_name = runtime_label_declaration_name field;
        source_type = field.ld_type;
      })
    fields

let resolve_source_record ~env ~unify_pat_types ~loc ~record_ty
    ~(rest_type_lid : Longident.t Location.loc) ~rest_type_expr ~rest_decl =
  match
    try
      Some
        (extract_instantiated_concrete_typedecl ~unify_pat_types env loc
           record_ty)
    with Not_found -> None
  with
  | Some source_decl -> (
    match source_decl.type_kind with
    | Type_record (fields, repr) -> (source_fields_of_decl fields, repr)
    | _ -> assert false)
  | None -> (
    unify_pat_types rest_type_lid.loc env record_ty rest_type_expr;
    match rest_decl.type_kind with
    | Type_record (fields, repr) -> (source_fields_of_decl fields, repr)
    | _ -> assert false)

let runtime_excluded_labels ~explicit_runtime_labels source_repr =
  match source_repr with
  | Record_inlined {attrs; _}
    when not (Ast_untagged_variants.process_untagged attrs) ->
    let tag_name =
      match Ast_untagged_variants.process_tag_name attrs with
      | Some s -> s
      | None -> "TAG"
    in
    if List.mem tag_name explicit_runtime_labels then explicit_runtime_labels
    else tag_name :: explicit_runtime_labels
  | _ -> explicit_runtime_labels

let type_record_pat_rest ~env ~pattern_force ~loc ~record_ty ~lbl_pat_list ~rest
    ~enter_variable ~unify_pat_types ~check_not_private =
  let rest_type_lid, rest_type_args_syntax =
    match rest.Parsetree.rest_type with
    | None ->
      raise_error rest.rest_loc env
        (Requires_type_annotation rest.rest_name.txt)
    | Some {ptyp_desc = Ptyp_constr (lid, type_args); _} -> (lid, type_args)
    | Some _ -> raise_error rest.rest_loc env Invalid_type
  in
  let rest_path, rest_annotation_decl =
    Typetexp.find_type env rest_type_lid.loc rest_type_lid.txt
  in
  let rest_annotation_decl = Ctype.instance_declaration rest_annotation_decl in
  let rest_type_args =
    type_args_from_annotation ~env ~pattern_force ~rest_type_lid
      rest_annotation_decl rest_type_args_syntax
  in
  let rest_type_expr =
    Btype.newgenty (Tconstr (rest_path, rest_type_args, ref Mnil))
  in
  check_not_private rest_type_lid.loc rest_type_expr rest_annotation_decl;
  List.iter2
    (fun param arg -> unify_pat_types rest_type_lid.loc env param arg)
    rest_annotation_decl.type_params rest_type_args;
  let rest_decl =
    match
      try
        Some
          (extract_instantiated_concrete_typedecl ~unify_pat_types env
             rest_type_lid.loc rest_type_expr)
      with Not_found -> None
    with
    | Some rest_decl -> (
      check_not_private rest_type_lid.loc rest_type_expr rest_decl;
      match rest_decl.type_kind with
      | Type_record _ -> rest_decl
      | _ -> raise_error rest_type_lid.loc env (Not_record rest_type_lid.txt))
    | None -> raise_error rest_type_lid.loc env (Not_record rest_type_lid.txt)
  in
  let explicit_fields =
    List.map (fun (_, label, _, _) -> label.lbl_name) lbl_pat_list
  in
  let explicit_runtime_labels =
    List.map
      (fun (_, label, _, _) -> runtime_label_description_name label)
      lbl_pat_list
  in
  let explicit_optional_fields =
    List.filter_map
      (fun (_, label, _, optional) ->
        if optional then Some label.lbl_name else None)
      lbl_pat_list
  in
  let rest_labels =
    match rest_decl.type_kind with
    | Type_record (labels, _) -> labels
    | _ -> assert false
  in
  let rest_field_names =
    List.map (fun label -> Ident.name label.ld_id) rest_labels
  in
  let source_fields, source_repr =
    resolve_source_record ~env ~unify_pat_types ~loc ~record_ty ~rest_type_lid
      ~rest_type_expr ~rest_decl
  in
  let not_optional =
    List.filter
      (fun rest_field ->
        List.mem rest_field explicit_fields
        && not (List.mem rest_field explicit_optional_fields))
      rest_field_names
  in
  if not_optional <> [] then
    raise_error rest.rest_loc env
      (Field_not_optional (not_optional, rest_type_lid.txt));
  let source_field_names =
    List.map (fun field -> field.source_name) source_fields
  in
  let missing =
    List.filter
      (fun source_field ->
        (not (List.mem source_field explicit_fields))
        && not (List.mem source_field rest_field_names))
      source_field_names
  in
  if missing <> [] then
    raise_error rest.rest_loc env (Field_missing (missing, rest_type_lid.txt));
  List.iter
    (fun (rest_label : label_declaration) ->
      let rest_field = Ident.name rest_label.ld_id in
      let rest_runtime_name = runtime_label_declaration_name rest_label in
      match
        Ext_list.find_first source_fields (fun field ->
            field.source_name = rest_field)
      with
      | None ->
        raise_error rest_type_lid.loc env
          (Extra_field (rest_field, rest_type_lid.txt))
      | Some source_field ->
        if source_field.source_runtime_name <> rest_runtime_name then
          raise_error rest_type_lid.loc env
            (Field_runtime_name_mismatch
               {
                 field = rest_field;
                 rest_type = rest_type_lid.txt;
                 source_runtime_name = source_field.source_runtime_name;
                 rest_runtime_name;
               });
        unify_pat_types rest_type_lid.loc env rest_label.ld_type
          source_field.source_type)
    rest_labels;
  if
    rest_field_names <> []
    && List.for_all
         (fun field -> List.mem field explicit_fields)
         rest_field_names
  then Location.prerr_warning rest.rest_loc Warnings.Bs_record_rest_empty;
  let rest_ident = enter_variable rest.rest_loc rest.rest_name rest_type_expr in
  {
    Typedtree.rest_ident;
    rest_name = rest.rest_name;
    rest_type = rest_type_expr;
    excluded_runtime_labels =
      runtime_excluded_labels ~explicit_runtime_labels source_repr;
  }

let report_error ppf = function
  | Invalid_type ->
    fprintf ppf "Record rest pattern must have the form: ...Type.t as name"
  | Requires_type_annotation name ->
    fprintf ppf
      "Record rest pattern `...%s` requires a type annotation. Use `...Type.t \
       as %s`."
      name name
  | Not_record lid ->
    fprintf ppf
      "Type %a is not a record type and cannot be used as a record rest \
       pattern."
      Printtyp.longident lid
  | Field_not_optional (fields, lid) -> (
    let field_list =
      fields |> List.map (fun field -> "\n- " ^ field) |> String.concat ""
    in
    match fields with
    | [field] ->
      fprintf ppf
        "The following field appears in both the explicit pattern and the rest \
         type `%a`:%s\n\n\
         Mark it as optional (`?%s`) in the explicit pattern."
        Printtyp.longident lid field_list field
    | _ ->
      fprintf ppf
        "The following fields appear in both the explicit pattern and the rest \
         type `%a`:%s\n\n\
         Mark them as optional (e.g. `?fieldName`) in the explicit pattern."
        Printtyp.longident lid field_list)
  | Field_missing (fields, lid) -> (
    let field_list =
      fields |> List.map (fun field -> "\n- " ^ field) |> String.concat ""
    in
    match fields with
    | [_] ->
      fprintf ppf
        "The following field is not part of the rest type `%a`:%s\n\n\
         List this field in the record pattern before the spread so it's not \
         present in the rest record."
        Printtyp.longident lid field_list
    | _ ->
      fprintf ppf
        "The following fields are not part of the rest type `%a`:%s\n\n\
         List these fields in the record pattern before the spread so they're \
         not present in the rest record."
        Printtyp.longident lid field_list)
  | Extra_field (field, lid) ->
    fprintf ppf
      "Field `%s` in the rest type `%a` does not exist in the source record \
       type."
      field Printtyp.longident lid
  | Field_runtime_name_mismatch
      {field; rest_type; source_runtime_name; rest_runtime_name} ->
    fprintf ppf
      "Field `%s` in the rest type `%a` has runtime representation `%s`, but \
       in the source record type it is `%s`. Runtime representations must \
       match."
      field Printtyp.longident rest_type rest_runtime_name source_runtime_name
