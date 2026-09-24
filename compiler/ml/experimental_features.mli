type feature = LetUnwrap

val enable_from_string : string -> unit

(* Isolate enabled features for one compiler request. *)
val with_fresh : (unit -> 'a) -> 'a
val is_enabled : feature -> bool
val to_string : feature -> string
val reset : unit -> unit
