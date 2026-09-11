val resolve_program :
  path_separator:char ->
  executable_extensions:(program:string -> string list) ->
  search_directories:(cwd:string -> string list -> string list) ->
  executable_is_usable:(string -> bool) ->
  cwd:string ->
  string ->
  string

val process_is_active : probe:(int -> bool) -> string -> bool
