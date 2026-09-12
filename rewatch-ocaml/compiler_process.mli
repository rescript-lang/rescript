val retain_critical_external_warnings : string -> string
val parse_job :
  bsc:string -> build_dir:string -> config:Config.t -> string -> Process.job
val ast_dependencies : build_dir:string -> string -> string list

val namespace_task :
  bsc:string ->
  runtime:string ->
  build_dir:string ->
  ocaml_dir:string ->
  entry:string option ->
  package_dirty:bool ->
  string ->
  Source.module_ list ->
  Compiler_scheduler.namespace_task option

val compile_job :
  bsc:string ->
  build_dir:string ->
  config:Config.t ->
  common_args:string list ->
  Source.module_ ->
  source_kind:Source.source_kind ->
  string ->
  Process.job

val post_build_tasks :
  Config.t -> string -> Compiler_scheduler.post_build_task list

val publish :
  build_dir:string ->
  ocaml_dir:string ->
  is_local:bool ->
  config:Config.t ->
  source_kind:Source.source_kind ->
  string ->
  Process.result ->
  Compiler_scheduler.publish_result
