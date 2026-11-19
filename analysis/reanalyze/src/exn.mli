type t

val compare : t -> t -> int
val make : id:string -> name:string -> t
val from_string : string -> t
val match_failure : t
val to_string : t -> string

val of_constructor : Longident.t -> Types.constructor_description -> t
(** The exception a pattern or expression constructs, named as written *)

val with_name : string -> t -> t

module Table : Hashtbl.S with type key = t
