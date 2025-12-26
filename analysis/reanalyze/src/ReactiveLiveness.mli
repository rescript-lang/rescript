(** Reactive liveness computation using fixpoint.
    
    Computes the set of live declarations incrementally. *)

type t = {
  live: (Lexing.position, unit) Reactive.t;
  edges: (Lexing.position, Lexing.position list) Reactive.t;
  roots: (Lexing.position, unit) Reactive.t;
}

val create : merged:ReactiveMerge.t -> t
(** [create ~merged] computes reactive liveness from merged DCE data.
    
    Returns a record containing:
    - live: positions that are live (via fixpoint)
    - edges: declaration → referenced positions
    - roots: initial live positions (annotated + externally referenced)
    
    Updates automatically when any input changes. *)

val print_stats : t:t -> unit
(** Print update statistics for liveness collections (roots, edges, live fixpoint) *)
