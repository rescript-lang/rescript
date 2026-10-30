type entry = {path: string; modified: float}
(** This index avoids repeatedly scanning [lib/ocaml] while retaining the source
    provenance embedded in AST headers. Mutating refresh operations must be
    called whenever publication changes the corresponding filesystem entry. *)

type ast_source = {ast_path: string; source_path: string}
type t

val is_managed_basename : string -> bool
val create : string list -> t
val files : t -> string -> string list
val ast_sources : t -> string -> ast_source list
val ast_dependencies : t -> string -> string list
val ast : t -> string -> entry option
val cmi : t -> string -> entry option
val cmt : t -> string -> entry option

val published : t -> string -> float option
(** When the module's CMI, or its CMJ if newer, was last written; [None] without
    a CMI. A CMJ can change without its CMI, for example when an exported inline
    body changes, and dependents compiled before it are stale too. *)

val refresh_cmi : t -> key:string -> path:string -> unit
(** Also refreshes the CMJ next to [path]. *)

val refresh_cmt : t -> key:string -> path:string -> unit
val refresh_ast : t -> source:string -> path:string -> unit
