type context = {
  build_root: string;
  bsc_path: string;
  bsc_hash: string;
  runtime_path: string;
  source_map_args: string list;
  package_output_specs: package_output_spec list;
}

and package_output_spec = {
  module_format: string;
  in_source: bool;
  suffix: string;
}

val package_output_specs : Config.t -> package_output_spec list

val make_context :
  build_root:string ->
  bsc_path:string ->
  runtime_path:string ->
  source_map_args:string list ->
  package_output_specs:package_output_spec list ->
  context

val path : string -> string
val owns_outputs : Config.t -> bool
val matches : context -> Config.t -> bool

val changed_package_output_specs :
  context -> Config.t -> package_output_spec list option

val config_with_package_output_specs :
  Config.t -> package_output_spec list -> Config.t

val needs_clean : context -> Config.t -> bool
val clean_package : Config.t -> unit
val verify_package : context -> Config.t -> bool
val write_package : context -> Config.t -> unit
