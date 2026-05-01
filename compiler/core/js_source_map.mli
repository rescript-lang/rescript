type t

val make :
  generated_file:string -> source_root:string -> sources_content:bool -> t

val with_builder : t option -> (unit -> 'a) -> 'a

val source_loc_of_loc : Location.t -> Location.t option

val mark_source_loc : Ext_pp.t -> Location.t option -> unit

val json : t -> string

val linked_comment : map_file:string -> string
