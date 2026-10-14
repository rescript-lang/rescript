exception Error of string

val platform_package : system:string -> architecture:string -> string option

val bundled_bsc :
  cwd:string ->
  executable:string ->
  root:string ->
  find_package:(string -> string -> string option) ->
  string
(** [find_package directory name] resolves package [name] from [directory]. *)

val runtime : find_package:(string -> string option) -> string
