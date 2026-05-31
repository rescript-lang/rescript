(** AST traversal to collect source annotations (@dead, @live, @genType).
    
    Traverses the typed AST and records annotations in a FileAnnotations.builder. *)

val structure :
  state:File_annotations.builder ->
  config:Dce_config.t ->
  do_gen_type:bool ->
  Typedtree.structure ->
  unit
(** Traverse a structure and collect annotations. *)

val signature :
  state:File_annotations.builder ->
  config:Dce_config.t ->
  Typedtree.signature ->
  unit
(** Traverse a signature and collect annotations. *)
