(** Dict patterns in exhaustiveness checking and match compilation *)

val lowering : Typedtree.pattern list -> Typedtree.pattern -> Typedtree.pattern
(** [lowering pats] turns the dict patterns of a match whose patterns are
    [pats] into record patterns of the [dict] type, with an optional field per
    key. It's the identity when they have none. *)
