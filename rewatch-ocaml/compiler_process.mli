val retain_critical_external_warnings : string -> string
val build_identity : string

type request = {args: string list; cwd: string}
(** A request for the embedded compiler: [bsc]'s arguments, ending with the
    input file, and the directory it runs in. *)

val is_held : string -> bool
(** The outputs a build holds in memory until their request finished (see
    {!Build_file_store.set_held_writes}): ASTs, interfaces and typed trees. *)

val parse_request : artifact_dir:string -> config:Config.t -> string -> request
val run : ?poll:(unit -> unit) -> request -> Process.result

val run_requests : request list -> Process.result list

val publish_parse : artifact_dir:string -> string -> unit
(** Writes the AST of a successful parse to the artifact directory. *)

val run_parses :
  ?poll:(unit -> unit) ->
  ?on_complete:(int -> unit) ->
  (string * Config.t * string) list ->
  Process.result list
(** Parses each [(artifact_dir, config, path)], publishing every successful
    parse that printed nothing with {!publish_parse} on the worker that parsed
    it, and discarding every failed one. The caller publishes the parses that
    printed warnings. *)

val task : request -> Process.task

val namespace_task :
  runtime:string ->
  artifact_dir:string ->
  entry:string option ->
  package_dirty:bool ->
  force:bool ->
  dev_namespace:string option ->
  string ->
  Source.module_ list ->
  Compiler_scheduler.namespace_task option

val compile_request :
  artifact_dir:string ->
  config:Config.t ->
  common_args:string list ->
  Source.module_ ->
  source_kind:Source.source_kind ->
  string ->
  request

val post_build_tasks :
  Config.t -> string -> Compiler_scheduler.post_build_task list

val discard_compile : artifact_dir:string -> config:Config.t -> string -> unit
(** Leaves the previous interface of a compile that failed on disk, dates its
    AST back and publishes its partial typed tree. *)

val publish :
  artifact_dir:string ->
  is_local:bool ->
  config:Config.t ->
  has_interface:bool ->
  source_kind:Source.source_kind ->
  string ->
  Process.result ->
  Compiler_scheduler.publish_result
