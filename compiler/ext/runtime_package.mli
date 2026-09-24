val name : string
val get_path : unit -> string
val set_path : string -> unit

val reset_path : unit -> unit
(** Runtime path configuration belongs to the current compiler request. The
    immutable executable-derived default is used when no override is set. *)
