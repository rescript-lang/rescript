val source_is_newer : source:string -> artifact:string -> bool

val source_is_not_older_than_ast :
  Compile_assets.t ->
  root:string ->
  source_mtimes:(string, float) Hashtbl.t ->
  string ->
  bool

val published_ast_path : ocaml_dir:string -> string -> string
