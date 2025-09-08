type t
type eq = t * t

val apply_equations : eqs:eq list -> t -> eq list
val dot : string -> t -> t
val from_string : string -> t
val to_list : t -> string list
val to_string : t -> string
