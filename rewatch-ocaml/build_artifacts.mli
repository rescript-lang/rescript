type cleanup_result = {
  removed_modules: string list;
  previous_ast_count: int;
  present_public_outputs: (string, unit) Hashtbl.t;
}

val lib_path : string -> string -> string
val relative_output_directory : string -> Config.package_spec -> string
val published_ast_path : ocaml_dir:string -> string -> string

val generated_js_path : Config.t -> string -> Config.package_spec -> string

val generated_build_js_path :
  build_dir:string -> Config.t -> string -> Config.package_spec -> string

val remove_public_outputs : Config.t -> string list -> unit

val cleanup_stale :
  ?ocaml_files:string list ->
  ?ast_sources:Compile_assets.ast_source list ->
  ?source_files:string list ->
  ?present_source_files:string list ->
  on_removed_module:(string -> unit) ->
  on_deferred_artifact:(string -> unit) ->
  root:string ->
  ocaml_dir:string ->
  is_local:bool ->
  Config.t ->
  Source.module_ list ->
  cleanup_result
