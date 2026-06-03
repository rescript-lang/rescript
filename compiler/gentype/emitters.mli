type t

val initial : t
val export_early : emitters:t -> string -> t
val require_early : emitters:t -> string -> t
val export : emitters:t -> string -> t
val import : emitters:t -> string -> t
val require : emitters:t -> string -> t
val to_string : separator:string -> t -> string
