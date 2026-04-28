(** AST traversal to collect source annotations (@dead, @live, @genType).
    
    Traverses the typed AST and records annotations in a FileAnnotations.builder. *)

val structure :
  state:FileAnnotations.builder ->
  config:DceConfig.t ->
  doGenType:bool ->
  Typedtree.structure ->
  unit
(** Traverse a structure and collect annotations. *)

val signature :
  state:FileAnnotations.builder ->
  config:DceConfig.t ->
  Typedtree.signature ->
  unit
(** Traverse a signature and collect annotations. *)
