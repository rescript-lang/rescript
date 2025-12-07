(** AST traversal to collect source annotations (@dead, @live, @genType).
    
    This module traverses the typed AST to find attribute annotations
    and records them in a FileAnnotations.builder. *)

open DeadCommon

let processAttributes ~state ~config ~doGenType ~name ~pos attributes =
  let getPayloadFun f = attributes |> Annotation.getAttributePayload f in
  let getPayload (x : string) =
    attributes |> Annotation.getAttributePayload (( = ) x)
  in
  if
    doGenType
    && getPayloadFun Annotation.tagIsOneOfTheGenTypeAnnotations <> None
  then FileAnnotations.annotate_gentype state pos;
  if getPayload WriteDeadAnnotations.deadAnnotation <> None then
    FileAnnotations.annotate_dead state pos;
  let nameIsInLiveNamesOrPaths () =
    config.DceConfig.cli.live_names |> List.mem name
    ||
    let fname =
      match Filename.is_relative pos.pos_fname with
      | true -> pos.pos_fname
      | false -> Filename.concat (Sys.getcwd ()) pos.pos_fname
    in
    let fnameLen = String.length fname in
    config.DceConfig.cli.live_paths
    |> List.exists (fun prefix ->
           String.length prefix <= fnameLen
           &&
           try String.sub fname 0 (String.length prefix) = prefix
           with Invalid_argument _ -> false)
  in
  if getPayload liveAnnotation <> None || nameIsInLiveNamesOrPaths () then
    FileAnnotations.annotate_live state pos;
  if attributes |> Annotation.isOcamlSuppressDeadWarning then
    FileAnnotations.annotate_live state pos

let collectExportLocations ~state ~config ~doGenType =
  let super = Tast_mapper.default in
  let currentlyDisableWarnings = ref false in
  let value_binding self
      ({vb_attributes; vb_pat} as value_binding : Typedtree.value_binding) =
    (match vb_pat.pat_desc with
    | Tpat_var (id, {loc = {loc_start = pos}})
    | Tpat_alias ({pat_desc = Tpat_any}, id, {loc = {loc_start = pos}}) ->
      if !currentlyDisableWarnings then FileAnnotations.annotate_live state pos;
      vb_attributes
      |> processAttributes ~state ~config ~doGenType ~name:(id |> Ident.name)
           ~pos
    | _ -> ());
    super.value_binding self value_binding
  in
  let type_kind toplevelAttrs self (typeKind : Typedtree.type_kind) =
    (match typeKind with
    | Ttype_record labelDeclarations ->
      labelDeclarations
      |> List.iter
           (fun ({ld_attributes; ld_loc} : Typedtree.label_declaration) ->
             toplevelAttrs @ ld_attributes
             |> processAttributes ~state ~config ~doGenType:false ~name:""
                  ~pos:ld_loc.loc_start)
    | Ttype_variant constructorDeclarations ->
      constructorDeclarations
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
                     toplevelAttrs @ cd_attributes @ ld_attributes
                     |> processAttributes ~state ~config ~doGenType:false
                          ~name:"" ~pos:ld_loc.loc_start)
                   flds
               | Cstr_tuple _ -> ()
             in
             toplevelAttrs @ cd_attributes
             |> processAttributes ~state ~config ~doGenType:false ~name:""
                  ~pos:cd_loc.loc_start)
    | _ -> ());
    super.type_kind self typeKind
  in
  let type_declaration self (typeDeclaration : Typedtree.type_declaration) =
    let attributes = typeDeclaration.typ_attributes in
    let _ = type_kind attributes self typeDeclaration.typ_kind in
    typeDeclaration
  in
  let value_description self
      ({val_attributes; val_id; val_val = {val_loc = {loc_start = pos}}} as
       value_description :
        Typedtree.value_description) =
    if !currentlyDisableWarnings then FileAnnotations.annotate_live state pos;
    val_attributes
    |> processAttributes ~state ~config ~doGenType ~name:(val_id |> Ident.name)
         ~pos;
    super.value_description self value_description
  in
  let structure_item self (item : Typedtree.structure_item) =
    (match item.str_desc with
    | Tstr_attribute attribute
      when [attribute] |> Annotation.isOcamlSuppressDeadWarning ->
      currentlyDisableWarnings := true
    | _ -> ());
    super.structure_item self item
  in
  let structure self (structure : Typedtree.structure) =
    let oldDisableWarnings = !currentlyDisableWarnings in
    super.structure self structure |> ignore;
    currentlyDisableWarnings := oldDisableWarnings;
    structure
  in
  let signature_item self (item : Typedtree.signature_item) =
    (match item.sig_desc with
    | Tsig_attribute attribute
      when [attribute] |> Annotation.isOcamlSuppressDeadWarning ->
      currentlyDisableWarnings := true
    | _ -> ());
    super.signature_item self item
  in
  let signature self (signature : Typedtree.signature) =
    let oldDisableWarnings = !currentlyDisableWarnings in
    super.signature self signature |> ignore;
    currentlyDisableWarnings := oldDisableWarnings;
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

let structure ~state ~config ~doGenType structure =
  let mapper = collectExportLocations ~state ~config ~doGenType in
  structure |> mapper.structure mapper |> ignore

let signature ~state ~config signature =
  let mapper = collectExportLocations ~state ~config ~doGenType:true in
  signature |> mapper.signature mapper |> ignore
