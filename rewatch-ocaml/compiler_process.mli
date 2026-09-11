val retain_critical_external_warnings : string -> string
val parse_job : bsc:string -> build_dir:string -> config:Config.t -> string -> Process.job
val ast_dependencies : build_dir:string -> string -> string list

val namespace_job :
  bsc:string ->
  runtime:string ->
  build_dir:string ->
  ocaml_dir:string ->
  entry:string option ->
  package_dirty:bool ->
  string ->
  Source.module_ list ->
  (Process.job * (Process.result -> unit)) option

val compile_job :
  bsc:string ->
  runtime:string ->
  build_dir:string ->
  watch:bool ->
  config:Config.t ->
  dependency_dirs:string list ->
  Source.module_ ->
  is_interface:bool ->
  string ->
  Process.job

val publish :
  ?poll:(unit -> unit) ->
  build_dir:string ->
  ocaml_dir:string ->
  watch:bool ->
  watch_output_paths:(string, unit) Hashtbl.t ->
  is_local:bool ->
  config:Config.t ->
  is_interface:bool ->
  string ->
  Process.result ->
  string
