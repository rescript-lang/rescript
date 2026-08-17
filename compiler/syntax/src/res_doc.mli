type t

val nil : t
val line : t
val hard_line : t
val soft_line : t
val literal_line : t
val text : string -> t
val concat : t list -> t
val indent : t -> t
val if_breaks : t -> t -> t
val line_suffix : t -> t
val group : t -> t
val breakable_group : force_break:bool -> t -> t

(* `customLayout docs` will pick the layout that fits from `docs`.
 * This is a very expensive computation as every layout from the list
 * will be checked until one fits. *)
val custom_layout : t list -> t
val break_parent : t
val join : sep:t -> t list -> t

(* [(doc1, sep1); (doc2,sep2)] joins as doc1 sep1 doc2 *)
val join_with_sep : (t * t) list -> t

val space : t
val comma : t
val dot : t
val dotdot : t
val dotdotdot : t
val less_than : t
val greater_than : t
val lbrace : t
val rbrace : t
val lparen : t
val rparen : t
val lbracket : t
val rbracket : t
val question : t
val tilde : t
val equal : t
val trailing_comma : t

val will_break : t -> bool
(** [will_break doc] checks whether [doc] contains forced line breaks.

    Forced breaks are not propagated through [customLayout], because doing so
    would always select the last layout the algorithm tries. Consumers can use
    [will_break] to detect a forced break in a custom layout and explicitly add
    [breakParent] to propagate it to the parent document. *)

val to_string : width:int -> t -> string
val debug : t -> unit [@@live]
