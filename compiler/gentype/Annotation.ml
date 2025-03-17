type import = {import_path: ImportPath.t}

type attribute_payload =
  | BoolPayload of bool
  | FloatPayload of string
  | IdentPayload of Longident.t
  | IntPayload of string
  | StringPayload of string
  | TuplePayload of attribute_payload list
  | UnrecognizedPayload

type t = GenType | GenTypeOpaque | NoGenType

let to_string annotation =
  match annotation with
  | GenType -> "GenType"
  | GenTypeOpaque -> "GenTypeOpaque"
  | NoGenType -> "NoGenType"

let tag_is_gentype s = s = "genType" || s = "gentype"
let tag_is_gentype_as s = s = "genType.as" || s = "gentype.as"
let tag_is_as s = s = "as"
let tag_is_int s = s = "int"
let tag_is_string s = s = "string"

let tag_is_tag s = s = "tag"

let tag_is_unboxed s = s = "unboxed" || s = "ocaml.unboxed"
let tag_is_gentype_import s = s = "genType.import" || s = "gentype.import"
let tag_is_gentype_opaque s = s = "genType.opaque" || s = "gentype.opaque"

let tag_is_one_of_the_gentype_annotations s =
  tag_is_gentype s || tag_is_gentype_as s || tag_is_gentype_import s
  || tag_is_gentype_opaque s

let tag_is_gentype_ignore_interface s =
  s = "genType.ignoreInterface" || s = "gentype.ignoreInterface"

let tag_is_doc s = s = "res.doc"

let tag_is_intern_local s = s = "internal.local"

let rec get_attribute_payload check_text (attributes : Typedtree.attributes) =
  let rec from_expr (expr : Parsetree.expression) =
    match expr with
    | {pexp_desc = Pexp_constant (Pconst_string (s, _))} ->
      Some (StringPayload s)
    | {pexp_desc = Pexp_constant (Pconst_integer (n, _))} -> Some (IntPayload n)
    | {pexp_desc = Pexp_constant (Pconst_float (s, _))} -> Some (FloatPayload s)
    | {
     pexp_desc = Pexp_construct ({txt = Lident (("true" | "false") as s)}, _);
     _;
    } ->
      Some (BoolPayload (s = "true"))
    | {pexp_desc = Pexp_tuple exprs} ->
      let payloads =
        exprs |> List.rev
        |> List.fold_left
             (fun payloads expr ->
               match expr |> from_expr with
               | Some payload -> payload :: payloads
               | None -> payloads)
             []
      in
      Some (TuplePayload payloads)
    | {pexp_desc = Pexp_ident {txt}} -> Some (IdentPayload txt)
    | _ -> None
  in
  match attributes with
  | [] -> None
  | ({txt; loc}, payload) :: _tl when check_text txt -> (
    let payload =
      match payload with
      | PStr [] -> Some UnrecognizedPayload
      | PStr ({pstr_desc = Pstr_eval (expr, _)} :: _) -> expr |> from_expr
      | PStr ({pstr_desc = Pstr_extension _} :: _) -> Some UnrecognizedPayload
      | PStr ({pstr_desc = Pstr_value _} :: _) -> Some UnrecognizedPayload
      | PStr ({pstr_desc = Pstr_primitive _} :: _) -> Some UnrecognizedPayload
      | PStr ({pstr_desc = Pstr_type _} :: _) -> Some UnrecognizedPayload
      | PStr ({pstr_desc = Pstr_typext _} :: _) -> Some UnrecognizedPayload
      | PStr ({pstr_desc = Pstr_exception _} :: _) -> Some UnrecognizedPayload
      | PStr ({pstr_desc = Pstr_module _} :: _) -> Some UnrecognizedPayload
      | PStr ({pstr_desc = Pstr_recmodule _} :: _) -> Some UnrecognizedPayload
      | PStr ({pstr_desc = Pstr_modtype _} :: _) -> Some UnrecognizedPayload
      | PStr ({pstr_desc = Pstr_open _} :: _) -> Some UnrecognizedPayload
      | PStr ({pstr_desc = Pstr_include _} :: _) -> Some UnrecognizedPayload
      | PStr ({pstr_desc = Pstr_attribute _} :: _) -> Some UnrecognizedPayload
      | PPat _ -> Some UnrecognizedPayload
      | PSig _ -> Some UnrecognizedPayload
      | PTyp _ -> Some UnrecognizedPayload
    in
    match payload with
    | None -> None
    | Some payload -> Some (loc, payload))
  | _hd :: tl -> get_attribute_payload check_text tl

let get_gentype_as_renaming attributes =
  match attributes |> get_attribute_payload tag_is_gentype_as with
  | Some (_, StringPayload s) -> Some s
  | None -> (
    match attributes |> get_attribute_payload tag_is_gentype with
    | Some (_, StringPayload s) -> Some s
    | _ -> None)
  | _ -> None

(* This is not supported anymore: only use to give a warning *)
let check_unsupported_gentype_as_renaming attributes =
  let error ~loc =
    Log_.Color.setup ();
    Log_.info ~loc ~name:"Warning genType" (fun ppf () ->
        Format.fprintf ppf
          "@\n\
           @genType.as is not supported anymore in type definitions. Use @as \
           from the language.")
  in
  match attributes |> get_attribute_payload tag_is_gentype_as with
  | Some (loc, _) -> error ~loc
  | None -> (
    match attributes |> get_attribute_payload tag_is_gentype with
    | Some (loc, _) -> error ~loc
    | None -> ())

let get_as_string attributes =
  match attributes |> get_attribute_payload tag_is_as with
  | Some (_, StringPayload s) -> Some s
  | _ -> None

let get_as_int attributes =
  match attributes |> get_attribute_payload tag_is_as with
  | Some (_, IntPayload s) -> (
    try Some (int_of_string s) with Failure _ -> None)
  | _ -> None

let get_attribute_import_renaming attributes =
  let attribute_import =
    attributes |> get_attribute_payload tag_is_gentype_import
  in
  let gentype_as_renaming = attributes |> get_gentype_as_renaming in
  match (attribute_import, gentype_as_renaming) with
  | Some (_, StringPayload import_string), _ ->
    (Some import_string, gentype_as_renaming)
  | ( Some
        ( _,
          TuplePayload
            [StringPayload import_string; StringPayload rename_string] ),
      _ ) ->
    (Some import_string, Some rename_string)
  | _ -> (None, gentype_as_renaming)

let get_tag attributes =
  match attributes |> get_attribute_payload tag_is_tag with
  | Some (_, StringPayload s) -> Some s
  | _ -> None

let get_doc_payload attributes =
  let doc_payload = attributes |> get_attribute_payload tag_is_doc in
  match doc_payload with
  | Some (_, StringPayload doc_string) when doc_string <> "" -> Some doc_string
  | _ -> None

let doc_string_from_attrs attributes = attributes |> get_doc_payload

let has_attribute check_text (attributes : Typedtree.attributes) =
  get_attribute_payload check_text attributes <> None

let from_attributes ~(config : GenTypeConfig.t) ~loc
    (attributes : Typedtree.attributes) =
  let default = if config.everything then GenType else NoGenType in
  if has_attribute tag_is_gentype_opaque attributes then GenTypeOpaque
  else if
    has_attribute (fun s -> tag_is_gentype s || tag_is_gentype_as s) attributes
  then (
    (match attributes |> get_attribute_payload tag_is_gentype with
    | Some (_, UnrecognizedPayload) -> ()
    | Some _ ->
      Log_.Color.setup ();
      Log_.info ~loc ~name:"Warning genType" (fun ppf () ->
          Format.fprintf ppf "Annotation payload is ignored")
    | _ -> ());
    GenType)
  else default

let rec module_type_check_annotation ~check_annotation
    ({mty_desc} : Typedtree.module_type) =
  match mty_desc with
  | Tmty_signature signature ->
    signature |> signature_check_annotation ~check_annotation
  | Tmty_ident _ | Tmty_functor _ | Tmty_with _ | Tmty_typeof _ | Tmty_alias _
    ->
    false

and module_type_declaration_check_annotation ~check_annotation
    ({mtd_type; mtd_attributes; mtd_loc = loc} :
      Typedtree.module_type_declaration) =
  mtd_attributes |> check_annotation ~loc
  ||
  match mtd_type with
  | None -> false
  | Some module_type ->
    module_type |> module_type_check_annotation ~check_annotation

and module_declaration_check_annotation ~check_annotation
    ({md_attributes; md_type; md_loc = loc} : Typedtree.module_declaration) =
  md_attributes |> check_annotation ~loc
  || md_type |> module_type_check_annotation ~check_annotation

and signature_item_check_annotation ~check_annotation
    (signature_item : Typedtree.signature_item) =
  match signature_item.sig_desc with
  | Tsig_type (_, type_declarations) ->
    type_declarations
    |> List.exists
         (fun ({typ_attributes; typ_loc = loc} : Typedtree.type_declaration) ->
           typ_attributes |> check_annotation ~loc)
  | Tsig_value {val_attributes; val_loc = loc} ->
    val_attributes |> check_annotation ~loc
  | Tsig_module module_declaration ->
    module_declaration |> module_declaration_check_annotation ~check_annotation
  | Tsig_attribute attribute ->
    [attribute] |> check_annotation ~loc:signature_item.sig_loc
  | Tsig_modtype module_type_declaration ->
    module_type_declaration
    |> module_type_declaration_check_annotation ~check_annotation
  | Tsig_typext _ | Tsig_exception _ | Tsig_recmodule _ | Tsig_open _
  | Tsig_include _ ->
    false

and signature_check_annotation ~check_annotation
    (signature : Typedtree.signature) =
  signature.sig_items
  |> List.exists (signature_item_check_annotation ~check_annotation)

let rec structure_item_check_annotation ~check_annotation
    (structure_item : Typedtree.structure_item) =
  match structure_item.str_desc with
  | Tstr_type (_, type_declarations) ->
    type_declarations
    |> List.exists
         (fun ({typ_attributes; typ_loc = loc} : Typedtree.type_declaration) ->
           typ_attributes |> check_annotation ~loc)
  | Tstr_value (_loc, value_bindings) ->
    value_bindings
    |> List.exists
         (fun ({vb_attributes; vb_loc = loc} : Typedtree.value_binding) ->
           vb_attributes |> check_annotation ~loc)
  | Tstr_primitive {val_attributes; val_loc = loc} ->
    val_attributes |> check_annotation ~loc
  | Tstr_module module_binding ->
    module_binding |> module_binding_check_annotation ~check_annotation
  | Tstr_recmodule module_bindings ->
    module_bindings
    |> List.exists (module_binding_check_annotation ~check_annotation)
  | Tstr_include {incl_attributes; incl_mod; incl_loc = loc} ->
    incl_attributes |> check_annotation ~loc
    || incl_mod |> module_expr_check_annotation ~check_annotation
  | Tstr_modtype module_type_declaration ->
    module_type_declaration
    |> module_type_declaration_check_annotation ~check_annotation
  | Tstr_attribute attribute ->
    [attribute] |> check_annotation ~loc:structure_item.str_loc
  | Tstr_eval _ | Tstr_typext _ | Tstr_exception _ | Tstr_open _ -> false

and module_expr_check_annotation ~check_annotation
    (module_expr : Typedtree.module_expr) =
  match module_expr.mod_desc with
  | Tmod_structure structure ->
    structure |> structure_check_annotation ~check_annotation
  | Tmod_constraint
      (module_expr, _moduleType, module_type_constraint, _moduleCoercion) -> (
    module_expr |> module_expr_check_annotation ~check_annotation
    ||
    match module_type_constraint with
    | Tmodtype_explicit module_type ->
      module_type |> module_type_check_annotation ~check_annotation
    | Tmodtype_implicit -> false)
  | Tmod_ident _ | Tmod_functor _ | Tmod_apply _ | Tmod_unpack _ -> false

and module_binding_check_annotation ~check_annotation
    ({mb_expr; mb_attributes; mb_loc = loc} : Typedtree.module_binding) =
  mb_attributes |> check_annotation ~loc
  || mb_expr |> module_expr_check_annotation ~check_annotation

and structure_check_annotation ~check_annotation
    (structure : Typedtree.structure) =
  structure.str_items
  |> List.exists (structure_item_check_annotation ~check_annotation)

let import_from_string import_string : import =
  let import_path = ImportPath.from_string_unsafe import_string in
  {import_path}

let update_config_for_module ~(config : GenTypeConfig.t) attributes =
  if attributes |> has_attribute tag_is_gentype then
    {config with everything = true}
  else config
