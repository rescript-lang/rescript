module String_set : Set.S with type elt = string

type module_kind = Source_module | Namespace_map
type cmi_change = Cmi_changed | Cmi_unchanged | Cmi_change_unknown

type module_ = {
  key: string;
  kind: module_kind;
  mutable dependencies: string list;
  mutable dependents: String_set.t;
  mutable compile_dirty: bool;
  mutable last_compiled_cmi: float option;
  mutable last_compiled_cmt: float option;
}

type t

val create : int -> t

val add :
  t ->
  key:string ->
  kind:module_kind ->
  last_compiled_cmi:float option ->
  last_compiled_cmt:float option ->
  unit

val find : t -> string -> module_ option
val find_exn : t -> string -> module_
val has_complete_compile_assets : module_ -> bool
val dependency_tree_compiled_after :
  ?namespace_freshness:(string, float option) Hashtbl.t ->
  t ->
  module_ ->
  module_ ->
  bool
val set_dependencies : t -> key:string -> string list -> unit

val mark_dependents_compile_dirty :
  ?visited:(string, unit) Hashtbl.t -> t -> module_ -> unit

val record_published_cmi :
  ?dirty_propagation:(string, unit) Hashtbl.t ->
  t ->
  compile_assets:Compile_assets.t ->
  module_ ->
  path:string ->
  cmi_change ->
  unit

val record_successful_compile :
  compile_assets:Compile_assets.t -> module_ -> cmt_path:string -> unit
