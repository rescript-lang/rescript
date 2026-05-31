(** Per-file AST processing for dead code analysis.
    
    This module uses mutable builders during AST traversal
    and returns them for merging. The caller freezes the accumulated
    builders before passing to the solver. *)

type file_context = {
  source_path: string;
  module_name: string;
  is_interface: bool;
}
(** File context for processing *)

type file_data = {
  annotations: File_annotations.builder;
  decls: Declarations.builder;
  refs: References.builder;
  cross_file: Cross_file_items.builder;
  file_deps: File_deps.builder;
}
(** Result of processing a cmt file - annotations, declarations, references, cross-file items, and file dependencies *)

val process_cmt_file :
  config:Dce_config.t ->
  file:file_context ->
  cmt_file_path:string ->
  Cmt_format.cmt_infos ->
  file_data
(** Process a cmt file and return mutable builders.
    Caller should merge builders and freeze before passing to solver. *)
