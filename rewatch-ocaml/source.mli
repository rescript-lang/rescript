(** Discovery keeps filesystem files separate from compilable modules. Commands
    such as formatting can operate on orphan interfaces and duplicate basenames,
    while compilation uses [module_] after pairing and duplicate validation. *)

type module_ = {
  name: string;
  implementation: string;
  interface: string option;
  is_dev: bool;
}

type discovery = {
  modules: module_ list;
  source_mtimes: (string * float) list;
  inventory_files: string list;
  present_files: string list;
  gentype_dirs: string list;
}

exception Error of string

type source_kind = Implementation | Interface

val source_kind : string -> source_kind option
val module_name : string -> string

val without_current_dir_segments : string -> string
(** [path] without its "." segments: "./src/./A.res" is "src/A.res". *)

val namespace_members :
  entry:string option -> dev:bool -> module_ list -> module_ list
(** The modules of the namespace map, or with [~dev:true] of the dev map. *)

val dev_namespace_compiler_name : Config.t -> module_ list -> string option
(** The dev map of a namespaced package with dev modules. *)

val duplicate_error :
  display_root:string -> string -> string -> string -> string -> exn

val resolve_active_features :
  Config.t -> string list -> (string, unit) Hashtbl.t

val active_sources :
  Config.t -> prod:bool -> features:string list option -> Config.source list

val discover_for_cleanup :
  on_missing:(string -> unit) -> Config.t -> prod:bool -> string list

val discover_files :
  on_missing:(string -> unit) ->
  Config.t ->
  prod:bool ->
  features:string list option ->
  string list

val discover_with_inventory :
  ?on_orphan:(string -> unit) ->
  on_missing:(string -> unit) ->
  ?display_root:string ->
  Config.t ->
  prod:bool ->
  features:string list option ->
  filter:Source_filter.t option ->
  discovery

val ast_path : string -> string
(** The file name, without directory, of a source's AST in the artifact
    directory: [A.ast] for an implementation, [A.iast] for an interface. *)

val compiler_basename : Config.t -> string -> string
val compiler_asset_basename : Config.t -> string -> string
