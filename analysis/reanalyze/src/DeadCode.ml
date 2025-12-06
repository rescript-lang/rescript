open DeadCommon

let processSignature ~config ~file ~doValues ~doTypes
    (signature : Types.signature) =
  signature
  |> List.iter (fun sig_item ->
         DeadValue.processSignatureItem ~config ~file ~doValues ~doTypes
           ~moduleLoc:Location.none
           ~path:[FileContext.module_name_tagged file]
           sig_item)

let processCmt ~config ~file ~cmtFilePath (cmt_infos : Cmt_format.cmt_infos) =
  (match cmt_infos.cmt_annots with
  | Interface signature ->
    ProcessDeadAnnotations.signature ~config signature;
    processSignature ~config ~file ~doValues:true ~doTypes:true
      signature.sig_type
  | Implementation structure ->
    let cmtiExists =
      Sys.file_exists ((cmtFilePath |> Filename.remove_extension) ^ ".cmti")
    in
    ProcessDeadAnnotations.structure ~config ~doGenType:(not cmtiExists)
      structure;
    processSignature ~config ~file ~doValues:true ~doTypes:false
      structure.str_type;
    let doExternals =
      (* This is already handled at the interface level, avoid issues in inconsistent locations
         https://github.com/BuckleScript/syntax/pull/54
         Ideally, the handling should be less location-based, just like other language aspects. *)
      false
    in
    DeadValue.processStructure ~config ~file ~doTypes:true ~doExternals
      ~cmt_value_dependencies:cmt_infos.cmt_value_dependencies structure
  | _ -> ());
  DeadType.TypeDependencies.forceDelayedItems ~config;
  DeadType.TypeDependencies.clear ()
