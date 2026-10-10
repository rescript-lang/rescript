val retain_critical_external_warnings : string -> string
val build_identity : string

type request = {args: string list; cwd: string}
(** A request for the embedded compiler: [bsc]'s arguments, ending with the
    input file, and the directory it runs in. *)

val parse_request : build_dir:string -> config:Config.t -> string -> request
val ast_dependencies : build_dir:string -> string -> string list
val run : ?poll:(unit -> unit) -> request -> Process.result
val run_requests :
  ?poll:(unit -> unit) ->
  ?on_complete:(int -> unit) ->
  request list ->
  Process.result list
val task : request -> Process.task

val namespace_task :
  runtime:string ->
  build_dir:string ->
  ocaml_dir:string ->
  entry:string option ->
  package_dirty:bool ->
  force:bool ->
  string ->
  Source.module_ list ->
  Compiler_scheduler.namespace_task option

val compile_request :
  build_dir:string ->
  config:Config.t ->
  common_args:string list ->
  Source.module_ ->
  source_kind:Source.source_kind ->
  string ->
  request

val post_build_tasks :
  Config.t -> string -> Compiler_scheduler.post_build_task list

val publish :
  build_dir:string ->
  ocaml_dir:string ->
  is_local:bool ->
  config:Config.t ->
  has_interface:bool ->
  source_kind:Source.source_kind ->
  string ->
  Process.result ->
  Compiler_scheduler.publish_result
