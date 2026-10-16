(** Fixed external arguments, written [@as(value) _]. *)

val of_payload : Parsetree.payload -> Parsetree.fixed_value Location.loc option
(** The fixed value denoted by an [@as] payload: an integer, a string, a
    non-interpolated backquoted string, or a [json] literal. *)

val to_expression :
  ?loc:Location.t -> Parsetree.fixed_value -> Parsetree.expression
(** The payload expression that spells [value]. *)

val to_attribute :
  ?loc:Location.t -> Parsetree.fixed_value Location.loc -> Parsetree.attribute
(** The [@as] attribute that spells [value], with its name located at [loc]. *)

val underscore_loc : Location.t -> Location.t
(** The location of the [_] that ends a fixed argument located at [loc]. *)

val as_name_loc : Location.t -> Location.t
(** The location of the [@as] that starts a fixed argument located at [loc]. *)

val normalize_external_type : Parsetree.core_type -> Parsetree.core_type
(** Turn the [@as(value) _] arguments of an external's type into
    [Parg_fixed]. *)
