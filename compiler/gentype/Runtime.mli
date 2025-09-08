open GenTypeCommon

type module_item
type module_access_path =
  | Root of string
  | Dot of module_access_path * module_item

val check_mutable_object_field : previous_name:string -> name:string -> bool
val default : string
val emit_module_access_path : config:Config.t -> module_access_path -> string

val is_mutable_object_field : string -> bool
val new_module_item : name:string -> module_item
val js_variant_tag : polymorphic:bool -> tag:string option -> string
val js_variant_payload_tag : n:int -> string
val js_variant_value : polymorphic:bool -> string
