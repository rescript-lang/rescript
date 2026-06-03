(** Per-file AST processing for dead code analysis.

    This module coordinates per-file processing using local mutable builders
    and returns them for merging. The caller freezes them before
    passing to the solver. *)

open Dead_common

(* ===== File context ===== *)

type file_context = {
  source_path: string;
  module_name: string;
  is_interface: bool;
}

let module_name_tagged (file : file_context) =
  file.module_name |> Name.create ~is_interface:file.is_interface

(* ===== Signature processing ===== *)

let process_signature ~config ~decls ~(file : file_context) ~do_values ~do_types
    (signature : Types.signature) =
  let dead_common_file : File_context.t =
    {
      source_path = file.source_path;
      module_name = file.module_name;
      is_interface = file.is_interface;
    }
  in
  signature
  |> List.iter (fun sig_item ->
         Dead_value.process_signature_item ~config ~decls ~file:dead_common_file
           ~do_values ~do_types ~module_loc:Location.none
           ~module_path:Module_path.initial
           ~path:[module_name_tagged file]
           sig_item)

(* ===== Main entry point ===== *)

type file_data = {
  annotations: File_annotations.builder;
  decls: Declarations.builder;
  refs: References.builder;
  cross_file: Cross_file_items.builder;
  file_deps: File_deps.builder;
}

let process_cmt_file ~config ~(file : file_context) ~cmt_file_path
    (cmt_infos : Cmt_format.cmt_infos) : file_data =
  (* Convert to DeadCommon.FileContext for functions that need it *)
  let dead_common_file : File_context.t =
    {
      source_path = file.source_path;
      module_name = file.module_name;
      is_interface = file.is_interface;
    }
  in
  (* Mutable builders for AST processing *)
  let annotations = File_annotations.create_builder () in
  let decls = Declarations.create_builder () in
  let refs = References.create_builder () in
  let cross_file = Cross_file_items.create_builder () in
  let file_deps = File_deps.create_builder () in
  (* Register this file *)
  File_deps.add_file file_deps file.source_path;
  (match cmt_infos.cmt_annots with
  | Interface signature ->
    Collect_annotations.signature ~state:annotations ~config signature;
    process_signature ~config ~decls ~file ~do_values:true ~do_types:true
      signature.sig_type
  | Implementation structure ->
    let cmti_exists =
      Sys.file_exists ((cmt_file_path |> Filename.remove_extension) ^ ".cmti")
    in
    Collect_annotations.structure ~state:annotations ~config
      ~do_gentype:(not cmti_exists) structure;
    process_signature ~config ~decls ~file ~do_values:true ~do_types:false
      structure.str_type;
    let do_externals = false in
    Dead_value.process_structure ~config ~decls ~refs ~file_deps ~cross_file
      ~file:dead_common_file ~do_types:true ~do_externals
      ~cmt_value_dependencies:cmt_infos.cmt_value_dependencies structure
  | _ -> ());
  (* Return builders - caller will merge and freeze *)
  {annotations; decls; refs; cross_file; file_deps}
