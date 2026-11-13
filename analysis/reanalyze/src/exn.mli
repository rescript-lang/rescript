type t

val compare : t -> t -> int
val from_lid : Longident.t -> t
val from_string : string -> t
val match_failure : t
val to_string : t -> string
