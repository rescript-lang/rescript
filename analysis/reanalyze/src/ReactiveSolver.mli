(** Reactive dead code solver.
    
    Reactive pipeline: decls + live → dead_decls, live_decls
    Issue generation uses DeadCommon.reportDeclaration for correct filtering.
    
    O(dead_decls + live_decls), not O(all_decls). *)

type t

val create :
  decls:(Lexing.position, Decl.t) Reactive.t ->
  live:(Lexing.position, unit) Reactive.t ->
  annotations:(Lexing.position, FileAnnotations.annotated_as) Reactive.t ->
  value_refs_from:(Lexing.position, PosSet.t) Reactive.t option ->
  config:DceConfig.t ->
  t

val collect_issues :
  t:t ->
  config:DceConfig.t ->
  ann_store:AnnotationStore.t ->
  Issue.t list
(** Collect issues. O(dead_decls + live_decls). *)

val stats : t:t -> int * int
(** (dead, live) counts *)
