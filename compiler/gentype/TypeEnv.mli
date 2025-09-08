open GenTypeCommon

type t

val add_module_equation : dep:dep -> internal:bool -> t -> unit
val add_module_path : type_env:t -> string -> ResolvedName.t
val add_type_equations : type_equations:(Longident.t * type_) list -> t -> t
val apply_type_equations : config:Config.t -> path:Path.t -> t -> type_ option
val expand_alias_to_external_module : name:string -> t -> dep option
val get_module_equations : t -> ResolvedName.eq list
val get_module_access_path : name:string -> t -> Runtime.module_access_path
val get_module : name:string -> t -> t option
val lookup : name:string -> t -> t option

val lookup_module_type_signature :
  path:Path.t -> t -> (Typedtree.signature * t) option

val new_module : name:string -> t -> t
val new_module_type : name:string -> signature:Typedtree.signature -> t -> t
val new_type : name:string -> t -> unit
val root : unit -> t
val to_string : t -> string
val update_module_item : module_item:Runtime.module_item -> t -> unit
