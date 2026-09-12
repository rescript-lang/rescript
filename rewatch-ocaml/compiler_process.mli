val retain_critical_external_warnings : string -> string
val parse_job :
  bsc:string -> build_dir:string -> config:Config.t -> string -> Process.job
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
  (Process.job * (Process.result -> Compiler_scheduler.publish_result)) option

val compile_job :
  bsc:string ->
  build_dir:string ->
  config:Config.t ->
  common_args:string list ->
  Source.module_ ->
  is_interface:bool ->
  string ->
  Process.job

val post_build_tasks : Config.t -> string -> (string * Process.task) list

val publish :
  build_dir:string ->
  ocaml_dir:string ->
  is_local:bool ->
  config:Config.t ->
  is_interface:bool ->
  string ->
  Process.result ->
  Compiler_scheduler.publish_result
