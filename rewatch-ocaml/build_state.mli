type module_ = {
  key: string;
  package_name: string;
  package_root: string;
  source: Source.module_;
  mutable raw_dependencies: string list;
  mutable dependencies: string list;
  mutable dependents: string list;
  mutable compile_dirty: bool;
  mutable deps_dirty: bool;
  mutable last_compiled_cmi: float option;
  mutable last_compiled_cmt: float option;
}

type t

val create : int -> t

val add :
  t ->
  key:string ->
  package_name:string ->
  package_root:string ->
  source:Source.module_ ->
  raw_dependencies:string list ->
  last_compiled_cmi:float option ->
  last_compiled_cmt:float option ->
  unit

val find : t -> string -> module_ option
val find_exn : t -> string -> module_
val has_complete_compile_assets : module_ -> bool
val dependency_compiled_after : module_ -> module_ -> bool
val set_dependencies : t -> key:string -> string list -> unit

val mark_dependents_compile_dirty :
  t -> module_ -> is_blocked:(string -> bool) -> unit
