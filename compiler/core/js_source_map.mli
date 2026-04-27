type t

val make :
  generated_file:string -> source_root:string -> sources_content:bool -> t

val with_builder : t option -> (unit -> 'a) -> 'a

val comment_of_loc : Location.t -> string option

val mark_comment : Ext_pp.t -> string -> bool

val json : t -> string

val linked_comment : map_file:string -> string
