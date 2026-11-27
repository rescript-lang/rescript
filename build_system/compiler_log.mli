val initialize : string -> unit
(** Compiler logs are finalized separately from artifact cleanup so one cleanup
    failure cannot leave a successfully completed log in its temporary state. *)

val append : string -> string -> unit
val finalize : string -> unit
