val retain_critical_external_warnings : string -> string
val build_identity : string

type request = {args: string list; cwd: string}
(** A request for the embedded compiler: [bsc]'s arguments, ending with the
    input file, and the directory it runs in. *)

val parse_request : build_dir:string -> config:Config.t -> string -> request
val run : ?poll:(unit -> unit) -> request -> Process.result

val run_requests :
  ?poll:(unit -> unit) ->
  ?on_complete:(int -> unit) ->
  request list ->
  Process.result list

val publish_parse :
  build_dir:string -> ocaml_dir:string -> config:Config.t -> string -> unit
(** Publishes the AST and the source of a successful parse into [ocaml_dir]. *)

val run_parses :
  ?poll:(unit -> unit) ->
  ?on_complete:(int -> unit) ->
  (string * string * Config.t * string) list ->
  Process.result list
(** Parses each [(build_dir, ocaml_dir, config, path)], publishing every
    successful parse that printed nothing with {!publish_parse} on the worker
    that parsed it. The caller publishes the parses that printed warnings. *)

val task : ?hooks:Compiler_scheduler.compile_hooks -> request -> Process.task

val cmi_redirect :
  cwd:string ->
  cmi_directories:string list ->
  wait_for_adoption:(unit -> unit) ->
  (string * string) list ->
  string ->
  string option
(** [cmi_redirect ~cwd ~cmi_directories ~wait_for_adoption redirects] maps a
    resolved .cmi candidate path to the contents redirected for it, matching
    paths as {!Build_file_store} does. [cmi_directories] are the directories in
    which the build publishes interfaces. A candidate that may name a
    redirected file under another spelling, because it differs from one only
    in case or lies in another directory under the same name, calls
    [wait_for_adoption] and is then read as usual. *)

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
