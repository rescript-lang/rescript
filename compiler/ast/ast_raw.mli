(** [%raw], [%ffi] and [%%raw]. *)

val extension_name : Parsetree.raw_kind -> string
(** ["raw"] or ["ffi"]. *)

val source_loc : Parsetree.raw_source -> Location.t

val js_source : Parsetree.raw_source -> string
(** The JavaScript source that a raw node stands for. *)

val payload_loc : Parsetree.raw_source -> Location.t
(** The location of the string or template literal holding the source. *)

val kind_of_name : string -> Parsetree.raw_kind option
(** The kind of [%raw] or [%ffi]. *)

val of_payload : Parsetree.payload -> Parsetree.raw_source option
(** The source in a parsed [%raw], [%ffi] or [%%raw] payload, if it is a plain
    string or template. *)

val to_extension :
  name:string ->
  name_loc:Location.t ->
  Parsetree.raw_source ->
  Parsetree.extension
(** The extension as parsed, for passes that treat raw JavaScript like the
    extension it is written as. *)

val uninterpreted : loc:Location.t -> string -> Location.error
(** The error for an extension that no compiler pass interpreted. *)
