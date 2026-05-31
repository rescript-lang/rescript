type t = Lsp.Uri.t

val from_path : string -> t
val from_string : string -> t
val is_interface : t -> bool
val strip_path : bool ref
val to_path : t -> string
val to_string : t -> string
val to_top_level_loc : t -> Location.t
val encode_u_r_i_component : string -> string
