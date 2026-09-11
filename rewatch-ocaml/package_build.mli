exception Parse_failure of string

val prepare_tree :
  seen:(string, unit) Hashtbl.t ->
  folder:string ->
  watch:bool ->
  stats:Build_types.t ->
  unit
