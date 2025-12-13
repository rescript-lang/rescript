(** Per-file AST processing for dead code analysis.
    
    This module coordinates per-file processing using local mutable builders
    and returns them for merging. The caller freezes them before
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

(* ===== Signature processing ===== *)

let processSignature ~config ~decls ~refs ~(file : file_context) ~doValues
    ~doTypes (signature : Types.signature) =
  let dead_common_file : FileContext.t =
    {
      source_path = file.source_path;
      module_name = file.module_name;
      is_interface = file.is_interface;
    }
  in
  signature
  |> List.iter (fun sig_item ->
         DeadValue.processSignatureItem ~config ~decls ~file:dead_common_file
           ~refs ~doValues ~doTypes ~moduleLoc:Location.none
           ~modulePath:ModulePath.initial
           ~path:[module_name_tagged file]
           sig_item)

(* ===== Main entry point ===== *)

type file_data = {
  annotations: FileAnnotations.builder;
  decls: Declarations.builder;
  refs: References.builder;
  cross_file: CrossFileItems.builder;
  file_deps: FileDeps.builder;
}

let process_cmt_file ~config ~(file : file_context) ~cmtFilePath
    (cmt_infos : Cmt_format.cmt_infos) : file_data =
  (* Convert to DeadCommon.FileContext for functions that need it *)
  let dead_common_file : FileContext.t =
    {
      source_path = file.source_path;
      module_name = file.module_name;
      is_interface = file.is_interface;
    }
  in
  (* Mutable builders for AST processing *)
  let annotations = FileAnnotations.create_builder () in
  let decls = Declarations.create_builder () in
  let refs = References.create_builder () in
  let cross_file = CrossFileItems.create_builder () in
  let file_deps = FileDeps.create_builder () in
  (* Register this file *)
  FileDeps.add_file file_deps file.source_path;
  (match cmt_infos.cmt_annots with
  | Interface signature ->
    CollectAnnotations.signature ~state:annotations ~config signature;
    processSignature ~config ~decls ~refs ~file ~doValues:true ~doTypes:true
      signature.sig_type
  | Implementation structure ->
    let cmtiExists =
      Sys.file_exists ((cmtFilePath |> Filename.remove_extension) ^ ".cmti")
    in
    CollectAnnotations.structure ~state:annotations ~config
      ~doGenType:(not cmtiExists) structure;
    processSignature ~config ~decls ~refs ~file ~doValues:true ~doTypes:false
      structure.str_type;
    let doExternals = false in
    DeadValue.processStructure ~config ~decls ~refs ~file_deps ~cross_file
      ~file:dead_common_file ~doTypes:true ~doExternals
      ~cmt_value_dependencies:cmt_infos.cmt_value_dependencies structure
  | _ -> ());
  (* Return builders - caller will merge and freeze *)
  {annotations; decls; refs; cross_file; file_deps}
