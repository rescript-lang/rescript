val retain_critical_external_warnings : string -> string
val build_identity : string
val parse_job :
  bsc:string -> build_dir:string -> config:Config.t -> string -> Process.job
val ast_dependencies :
  ?session:Rescript_compiler_driver.session ->
  build_dir:string ->
  string ->
  string list
val run :
  ?session:Rescript_compiler_driver.session ->
  ?poll:(unit -> unit) ->
  Process.job ->
  Process.result
val run_jobs :
  ?session:Rescript_compiler_driver.session ->
  ?poll:(unit -> unit) ->
  ?on_complete:(int -> unit) ->
  Process.job list ->
  Process.result list
val task :
  ?session:Rescript_compiler_driver.session -> Process.job -> Process.task

val namespace_task :
  ?session:Rescript_compiler_driver.session ->
  bsc:string ->
  runtime:string ->
  build_dir:string ->
  ocaml_dir:string ->
  entry:string option ->
  package_dirty:bool ->
  force:bool ->
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
  ?session:Rescript_compiler_driver.session ->
  retain_interface:bool ->
  dependencies:string list ->
  build_dir:string ->
  ocaml_dir:string ->
  is_local:bool ->
  config:Config.t ->
  source_kind:Source.source_kind ->
  string ->
  Process.result ->
  Compiler_scheduler.publish_result
