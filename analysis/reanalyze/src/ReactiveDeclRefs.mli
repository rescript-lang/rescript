(** Reactive mapping from declarations to their outgoing references.
    
    This is the reactive version of [Liveness.build_decl_refs_index].
    Updates incrementally when refs or declarations change.
    
    Next step: combine with a reactive fixpoint combinator for fully
    incremental liveness computation. *)

val create :
  decls:(Lexing.position, Decl.t) Reactive.t ->
  value_refs_from:(Lexing.position, PosSet.t) Reactive.t ->
  type_refs_from:(Lexing.position, PosSet.t) Reactive.t ->
  (Lexing.position, PosSet.t * PosSet.t) Reactive.t
(** [create ~decls ~value_refs_from ~type_refs_from] creates a reactive index
    mapping each declaration position to its outgoing references.
    
    Returns [(value_targets, type_targets)] for each declaration. *)
