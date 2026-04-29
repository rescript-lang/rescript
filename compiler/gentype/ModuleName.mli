type t

val compare : t -> t -> int
val curry : t
val for_js_file : t -> t
val for_inner_module : file_name:t -> inner_module_name:string -> t
val nested_make_hidden_export_access :
  file_name:t -> string list -> string option

val from_string_unsafe : string -> t
(** Used to turn strings read from external files into module names. *)

val rescript_pervasives : t
val to_string : t -> string
val uncapitalize : t -> t
