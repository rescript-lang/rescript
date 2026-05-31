(** AST traversal to collect source annotations (@dead, @live, @genType).
    
    This module traverses the typed AST to find attribute annotations
    and records them in a FileAnnotations.builder. *)

open DeadCommon

type scope_default = FileAnnotations.annotated_as option

let process_attributes ~(scope_default : scope_default) ~state ~config ~do_gen_type
    ~name ~pos attributes =
  (match scope_default with
  | Some FileAnnotations.Live -> FileAnnotations.annotate_live state pos
  | Some FileAnnotations.Dead -> FileAnnotations.annotate_dead state pos
  | Some FileAnnotations.GenType -> FileAnnotations.annotate_gentype state pos
  | None -> ());
  let get_payload_fun f = attributes |> Annotation.get_attribute_payload f in
  let get_payload (x : string) =
    attributes |> Annotation.get_attribute_payload (( = ) x)
  in
  if
    do_gen_type
    && get_payload_fun Annotation.tag_is_one_of_the_gen_type_annotations <> None
  then FileAnnotations.annotate_gentype state pos;
  if get_payload "dead" <> None then FileAnnotations.annotate_dead state pos;
  let name_is_in_live_names_or_paths () =
    config.DceConfig.cli.live_names |> List.mem name
    ||
    let fname =
      match Filename.is_relative pos.pos_fname with
      | true -> pos.pos_fname
      | false -> Filename.concat (Sys.getcwd ()) pos.pos_fname
    in
    let fname_len = String.length fname in
    config.DceConfig.cli.live_paths
    |> List.exists (fun prefix ->
           String.length prefix <= fname_len
           &&
           try String.sub fname 0 (String.length prefix) = prefix
           with Invalid_argument _ -> false)
  in
  if get_payload live_annotation <> None || name_is_in_live_names_or_paths () then
    FileAnnotations.annotate_live state pos;
  if attributes |> Annotation.is_ocaml_suppress_dead_warning then
    FileAnnotations.annotate_live state pos

let collect_export_locations ~state ~config ~do_gen_type =
  let super = Tast_mapper.default in
  let currently_disable_warnings = ref false in
  let current_scope_default : scope_default ref = ref None in

  let scope_default_from_toplevel_attribute (attribute : Parsetree.attribute) :
      scope_default =
    let attrs = [attribute] in
    let get_payload (x : string) =
      attrs |> Annotation.get_attribute_payload (( = ) x)
    in
    if get_payload "dead" <> None then Some FileAnnotations.Dead
    else if get_payload "live" <> None then Some FileAnnotations.Live
    else if get_payload "genType" <> None then Some FileAnnotations.GenType
    else None
  in

  let value_binding self
      ({vb_attributes; vb_pat} as value_binding : Typedtree.value_binding) =
    (match vb_pat.pat_desc with
    | Tpat_var (id, {loc = {loc_start = pos}})
    | Tpat_alias ({pat_desc = Tpat_any}, id, {loc = {loc_start = pos}}) ->
      if !currently_disable_warnings then FileAnnotations.annotate_live state pos;
      vb_attributes
      |> process_attributes ~scope_default:!current_scope_default ~state ~config
           ~do_gen_type ~name:(id |> Ident.name) ~pos
    | _ -> ());
    super.value_binding self value_binding
  in
  let type_kind toplevel_attrs self (type_kind : Typedtree.type_kind) =
    (match type_kind with
    | Ttype_record label_declarations ->
      label_declarations
      |> List.iter
           (fun ({ld_attributes; ld_loc} : Typedtree.label_declaration) ->
             toplevel_attrs @ ld_attributes
             |> process_attributes ~scope_default:!current_scope_default ~state
                  ~config ~do_gen_type:false ~name:"" ~pos:ld_loc.loc_start)
    | Ttype_variant constructor_declarations ->
      constructor_declarations
      |> List.iter
           (fun
             ({cd_attributes; cd_loc; cd_args} :
               Typedtree.constructor_declaration)
           ->
             let _process_inline_records =
               match cd_args with
               | Cstr_record flds ->
                 List.iter
                   (fun ({ld_attributes; ld_loc} : Typedtree.label_declaration)
                      ->
                     toplevel_attrs @ cd_attributes @ ld_attributes
                     |> process_attributes ~scope_default:!current_scope_default
                          ~state ~config ~do_gen_type:false ~name:""
                          ~pos:ld_loc.loc_start)
                   flds
               | Cstr_tuple _ -> ()
             in
             toplevel_attrs @ cd_attributes
             |> process_attributes ~scope_default:!current_scope_default ~state
                  ~config ~do_gen_type:false ~name:"" ~pos:cd_loc.loc_start)
    | _ -> ());
    super.type_kind self type_kind
  in
  let type_declaration self (type_declaration : Typedtree.type_declaration) =
    let attributes = type_declaration.typ_attributes in
    let _ = type_kind attributes self type_declaration.typ_kind in
    type_declaration
  in
  let value_description self
      ({val_attributes; val_id; val_val = {val_loc = {loc_start = pos}}} as
       value_description :
        Typedtree.value_description) =
    if !currently_disable_warnings then FileAnnotations.annotate_live state pos;
    val_attributes
    |> process_attributes ~scope_default:!current_scope_default ~state ~config
         ~do_gen_type ~name:(val_id |> Ident.name) ~pos;
    super.value_description self value_description
  in
  let structure_item self (item : Typedtree.structure_item) =
    (match item.str_desc with
    | Tstr_attribute attribute -> (
      match scope_default_from_toplevel_attribute attribute with
      | Some _ as new_default -> current_scope_default := new_default
      | None ->
        if [attribute] |> Annotation.is_ocaml_suppress_dead_warning then
          currently_disable_warnings := true)
    | _ -> ());
    super.structure_item self item
  in
  let structure self (structure : Typedtree.structure) =
    let old_disable_warnings = !currently_disable_warnings in
    let old_scope_default = !current_scope_default in
    super.structure self structure |> ignore;
    currently_disable_warnings := old_disable_warnings;
    current_scope_default := old_scope_default;
    structure
  in
  let signature_item self (item : Typedtree.signature_item) =
    (match item.sig_desc with
    | Tsig_attribute attribute -> (
      match scope_default_from_toplevel_attribute attribute with
      | Some _ as new_default -> current_scope_default := new_default
      | None ->
        if [attribute] |> Annotation.is_ocaml_suppress_dead_warning then
          currently_disable_warnings := true)
    | _ -> ());
    super.signature_item self item
  in
  let signature self (signature : Typedtree.signature) =
    let old_disable_warnings = !currently_disable_warnings in
    let old_scope_default = !current_scope_default in
    super.signature self signature |> ignore;
    currently_disable_warnings := old_disable_warnings;
    current_scope_default := old_scope_default;
    signature
  in
  {
    super with
    signature;
    signature_item;
    structure;
    structure_item;
    type_declaration;
    value_binding;
    value_description;
  }

let structure ~state ~config ~do_gen_type structure =
  let mapper = collect_export_locations ~state ~config ~do_gen_type in
  structure |> mapper.structure mapper |> ignore

let signature ~state ~config signature =
  let mapper = collect_export_locations ~state ~config ~do_gen_type:true in
  signature |> mapper.signature mapper |> ignore
