(** The frozen AST 0 file protocol used by external PPXs. The caller decides
    whether a PPX is present; this module never runs for ordinary requests. *)

val rewrite : 'a Ml_binary.kind -> string list -> 'a -> 'a
