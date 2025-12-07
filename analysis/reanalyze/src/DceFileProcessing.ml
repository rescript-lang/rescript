(** Per-file AST processing for dead code analysis.
    
    This module uses FileAnnotations.builder during AST traversal
    and returns it for merging. The caller freezes it before
    passing to the solver. *)

open DeadCommon

(* ===== File context ===== *)

type file_context = {
  source_path: string;
  module_name: string;
  is_interface: bool;
}

let module_name_tagged (file : file_context) =
  file.module_name |> Name.create ~isInterface:file.is_interface

(* ===== AST Processing (internal) ===== *)

module CollectAnnotations = struct
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
        if !currentlyDisableWarnings then
          FileAnnotations.annotate_live state pos;
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
                     (fun ({ld_attributes; ld_loc} :
                            Typedtree.label_declaration) ->
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
      |> processAttributes ~state ~config ~doGenType
           ~name:(val_id |> Ident.name) ~pos;
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
    let collectExportLocations =
      collectExportLocations ~state ~config ~doGenType
    in
    structure
    |> collectExportLocations.structure collectExportLocations
    |> ignore

  let signature ~state ~config signature =
    let collectExportLocations =
      collectExportLocations ~state ~config ~doGenType:true
    in
    signature
    |> collectExportLocations.signature collectExportLocations
    |> ignore
end

let processSignature ~config ~(file : file_context) ~doValues ~doTypes
    (signature : Types.signature) =
  let dead_common_file : FileContext.t =
    {
      source_path = file.source_path;
      module_name = file.module_name;
      is_interface = file.is_interface;
    }
  in
  signature
  |> List.iter (fun sig_item ->
         DeadValue.processSignatureItem ~config ~file:dead_common_file ~doValues
           ~doTypes ~moduleLoc:Location.none
           ~path:[module_name_tagged file]
           sig_item)

(* ===== Main entry point ===== *)

let process_cmt_file ~config ~(file : file_context) ~cmtFilePath
    (cmt_infos : Cmt_format.cmt_infos) : FileAnnotations.builder =
  (* Convert to DeadCommon.FileContext for functions that need it *)
  let dead_common_file : FileContext.t =
    {
      source_path = file.source_path;
      module_name = file.module_name;
      is_interface = file.is_interface;
    }
  in
  (* Mutable builder for AST processing *)
  let builder = FileAnnotations.create_builder () in
  (match cmt_infos.cmt_annots with
  | Interface signature ->
    CollectAnnotations.signature ~state:builder ~config signature;
    processSignature ~config ~file ~doValues:true ~doTypes:true
      signature.sig_type
  | Implementation structure ->
    let cmtiExists =
      Sys.file_exists ((cmtFilePath |> Filename.remove_extension) ^ ".cmti")
    in
    CollectAnnotations.structure ~state:builder ~config
      ~doGenType:(not cmtiExists) structure;
    processSignature ~config ~file ~doValues:true ~doTypes:false
      structure.str_type;
    let doExternals = false in
    DeadValue.processStructure ~config ~file:dead_common_file ~doTypes:true
      ~doExternals ~cmt_value_dependencies:cmt_infos.cmt_value_dependencies
      structure
  | _ -> ());
  DeadType.TypeDependencies.forceDelayedItems ~config;
  DeadType.TypeDependencies.clear ();
  (* Return builder - caller will merge and freeze *)
  builder
