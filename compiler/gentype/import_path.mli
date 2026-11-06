open Gentype_common

type t

val dump : t -> string

(* Escape a semantic import path for a single-quoted JavaScript/TypeScript
   string literal. The returned string does not include the quotes. *)
val emit : t -> string
val from_module : dir:string -> import_extension:string -> Module_name.t -> t
val from_string_unsafe : string -> t

val to_cmt : config:Config.t -> t -> string
(** The .cmt of an imported module of the same package, relative to the
    compiler's working directory, or "" for a module of another package. *)
