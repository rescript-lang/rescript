(** Per-file AST processing for dead code analysis.
    
    This module uses [FileAnnotations.builder] during AST traversal
    and returns it for merging. The caller freezes the accumulated
    builder before passing to the solver. *)

type file_context = {
  source_path: string;
  module_name: string;
  is_interface: bool;
}
(** File context for processing *)

val process_cmt_file :
  config:DceConfig.t ->
  file:file_context ->
  cmtFilePath:string ->
  Cmt_format.cmt_infos ->
  FileAnnotations.builder
(** Process a cmt file and return mutable builder.
    Caller should merge builders and freeze before passing to solver. *)
