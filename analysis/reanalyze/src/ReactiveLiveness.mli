(** Reactive liveness computation using fixpoint.
    
    Computes the set of live declarations incrementally. *)

val create : merged:ReactiveMerge.t -> (Lexing.position, unit) Reactive.t
(** [create ~merged] computes reactive liveness from merged DCE data.
    
    Returns a reactive collection where presence indicates the position is live.
    Updates automatically when any input changes. *)
