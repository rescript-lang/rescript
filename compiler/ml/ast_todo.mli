(** [%todo]. *)

val name : string
(** ["todo"]. *)

val of_payload : Parsetree.payload -> Parsetree.expression option option
(** The message of a parsed [%todo] payload: [Some None] for no payload,
    [Some (Some message)] for a string literal or a template without
    interpolations, [None] for any other payload, which stays an extension. *)

val to_extension :
  name_loc:Location.t -> Parsetree.expression option -> Parsetree.extension
(** The extension as parsed. *)
