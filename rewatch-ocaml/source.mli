type module_ = {
  name: string;
  implementation: string;
  interface: string option;
  is_dev: bool;
  feature: string option;
  mutable deps: string list;
}

type discovery = {
  modules: module_ list;
  source_mtimes: (string * float) list;
  inventory_files: string list;
  gentype_dirs: string list;
}

exception Error of string

val module_name : string -> string
val is_non_exotic_module_name : string -> bool

val duplicate_error :
  display_root:string -> string -> string -> string -> string -> exn

val resolve_active_features :
  Config.t -> string list -> (string, unit) Hashtbl.t

val discover_for_cleanup :
  ?on_missing:(string -> unit) ->
  Config.t ->
  prod:bool ->
  string list * string list

val discover_with_inventory :
  ?on_orphan:(string -> unit) ->
  ?on_missing:(string -> unit) ->
  ?display_root:string ->
  Config.t ->
  prod:bool ->
  features:string list option ->
  filter:Source_filter.t option ->
  discovery

val discover :
  ?on_orphan:(string -> unit) ->
  ?on_missing:(string -> unit) ->
  ?display_root:string ->
  Config.t ->
  prod:bool ->
  features:string list option ->
  filter:Source_filter.t option ->
  module_ list

val ast_path : string -> string
val compiler_basename : Config.t -> string -> string
val compiler_asset_basename : Config.t -> string -> string
