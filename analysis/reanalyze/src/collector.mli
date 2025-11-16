type t

val dead_common_sink : unit -> t
val collected : unit -> t

val add_decl : t -> Collected_types.decl -> unit
val find_decl : t -> Lexing.position -> Common.decl option
val replace_decl : t -> Common.decl -> unit
val add_value_reference : t -> Collected_types.value_reference -> unit
val add_type_reference : t -> Collected_types.type_reference -> unit
val finalize : t -> Collected_types.t
val tee : t -> t -> t

