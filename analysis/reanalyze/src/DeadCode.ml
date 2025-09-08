open DeadCommon

let processSignature ~doValues ~doTypes (signature : Types.signature) =
  signature
  |> List.iter (fun sig_item ->
         DeadValue.processSignatureItem ~doValues ~doTypes
           ~moduleLoc:Location.none
           ~path:[!Common.currentModuleName]
           sig_item)

let processCmt ~cmtFilePath (cmt_infos : Cmt_format.cmt_infos) =
  (match cmt_infos.cmt_annots with
  | Interface signature ->
    ProcessDeadAnnotations.signature signature;
    processSignature ~doValues:true ~doTypes:true signature.sig_type
  | Implementation structure ->
    let cmtiExists =
      Sys.file_exists ((cmtFilePath |> Filename.remove_extension) ^ ".cmti")
    in
    ProcessDeadAnnotations.structure ~doGenType:(not cmtiExists) structure;
    processSignature ~doValues:true ~doTypes:false structure.str_type;
    let doExternals =
      (* This is already handled at the interface level, avoid issues in inconsistent locations
         https://github.com/BuckleScript/syntax/pull/54
         Ideally, the handling should be less location-based, just like other language aspects. *)
      false
    in
    DeadValue.processStructure ~doTypes:true ~doExternals
      ~cmt_value_dependencies:cmt_infos.cmt_value_dependencies structure
  | _ -> ());
  DeadType.TypeDependencies.forceDelayedItems ();
  DeadType.TypeDependencies.clear ()
