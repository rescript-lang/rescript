module String_set : Set.S with type elt = string

type module_ = {
  key: string;
  package_name: string;
  package_root: string;
  source: Source.module_;
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
  package_name:string ->
  package_root:string ->
  source:Source.module_ ->
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
