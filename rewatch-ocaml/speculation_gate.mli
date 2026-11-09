(** Holds back a compiler request that started before its dependencies
    finished. The request computes freely but waits here before it writes any
    artifact or returns its result, until the scheduler either adopts it (the
    dependencies completed and the module would have been compiled now) or
    abandons it. *)

type t

exception Abandoned
(** Raised in the waiting request when the gate is abandoned or the request is
    cancelled before adoption. *)

val create : unit -> t

val open_ : t -> unit
(** Adopt the request. Opening an abandoned gate is an error. *)

val abandon : t -> unit
(** The request will not be adopted. Idempotent; no effect once open. *)

val cancel : t -> unit
(** Like {!abandon}, for interruption. No effect once open. *)

val is_abandoned : t -> bool

val wait : t -> unit
(** Block until the gate is opened; raise {!Abandoned} if it is abandoned or
    cancelled instead. Returns at once once open. *)
