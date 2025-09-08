open GenTypeCommon

type t

val bs_curry_path : config:Config.t -> t
val chop_extension_safe : t -> t [@@live]
val dump : t -> string
val emit : t -> string
val from_module : dir:string -> import_extension:string -> ModuleName.t -> t
val from_string_unsafe : string -> t
val to_cmt : config:Config.t -> output_file_relative:string -> t -> string
