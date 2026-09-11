type cleanup_result = {
  removed_modules: string list;
  previous_ast_count: int;
  deferred_artifacts: string list;
  present_public_outputs: (string, unit) Hashtbl.t;
}

val lib_path : string -> string -> string

val generated_js_path :
  Config.t -> string -> Config.package_spec -> string

val generated_build_js_path :
  build_dir:string -> Config.t -> string -> Config.package_spec -> string

val remove_public_outputs : Config.t -> Source.module_ list -> unit

val is_generated_output_path : string -> bool

val with_root_options : Config.t -> Config.t -> Config.t

val cleanup_stale :
  ?ocaml_files:string list ->
  ?ast_sources:(string * string) list ->
  ?source_files:string list ->
  root:string ->
  ocaml_dir:string ->
  is_local:bool ->
  Config.t ->
  Source.module_ list ->
  cleanup_result
