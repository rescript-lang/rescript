open Gentype_common

type t

val bs_curry_path : config:Config.t -> t
val chop_extension_safe : t -> t [@@live]
val dump : t -> string

(* Escape a semantic import path for a single-quoted JavaScript/TypeScript
   string literal. The returned string does not include the quotes. *)
val emit : t -> string
val from_module : dir:string -> import_extension:string -> Module_name.t -> t
val from_string_unsafe : string -> t
val to_cmt : config:Config.t -> output_file_relative:string -> t -> string
