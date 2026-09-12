type entry = {path: string; modified: float}
type t

val is_managed_basename : string -> bool
val create : string list -> t
val files : t -> string -> string list
val ast_sources : t -> string -> (string * string) list
val ast : t -> string -> entry option
val cmi : t -> string -> entry option
val cmt : t -> string -> entry option
val refresh_cmi : t -> key:string -> path:string -> unit
val refresh_cmt : t -> key:string -> path:string -> unit
val refresh_ast : t -> source:string -> path:string -> unit
