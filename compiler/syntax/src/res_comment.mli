type t

val to_string : t -> string

val loc : t -> Location.t
val txt : t -> string
val prev_tok_end_pos : t -> Lexing.position

val set_prev_tok_end_pos : t -> Lexing.position -> unit

val is_doc_comment : t -> bool

val is_module_comment : t -> bool

val is_single_line_comment : t -> bool

val make_single_line_comment : loc:Location.t -> string -> t
val make_multi_line_comment :
  loc:Location.t -> doc_comment:bool -> standalone:bool -> string -> t
val trim_spaces : string -> string
