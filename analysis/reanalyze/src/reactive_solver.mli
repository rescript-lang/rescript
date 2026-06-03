(** Reactive dead code solver.
    
    Reactive pipeline: decls + live → dead_decls, live_decls
    Issue generation uses DeadCommon.reportDeclaration for correct filtering.
    
    O(dead_decls + live_decls), not O(all_decls). *)

type t

val create :
  decls:(Lexing.position, Decl.t) Reactive.t ->
  live:(Lexing.position, unit) Reactive.t ->
  annotations:(Lexing.position, File_annotations.annotated_as) Reactive.t ->
  value_refs_from:(Lexing.position, Pos_set.t) Reactive.t option ->
  config:Dce_config.t ->
  t

val collect_issues :
  t:t -> config:Dce_config.t -> ann_store:Annotation_store.t -> Issue.t list
(** Collect issues. O(dead_decls + live_decls). *)

val iter_live_decls : t:t -> (Decl.t -> unit) -> unit
(** Iterate over live declarations *)

val is_pos_live : t:t -> Lexing.position -> bool
(** Check if a position is live using the reactive collection.
    Returns true if pos is not a declaration (matches non-reactive behavior). *)

val stats : t:t -> int * int
(** (dead, live) counts *)

val print_stats : t:t -> unit
(** Print update statistics for all reactive collections *)
