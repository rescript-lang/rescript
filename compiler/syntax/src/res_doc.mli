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

val break_before_line_suffixes : t -> t
(** [break_before_line_suffixes doc] makes every line suffix in [doc] break the
    groups around it, so that a trailing line comment stays where it was written
    instead of moving to the end of the line. Use it for a doc that is followed
    by an opening brace on the same line, where the comment would move into the
    block. *)

val has_ungrouped_line_suffix : t -> bool
(** [has_ungrouped_line_suffix doc] checks whether [doc] contains a line suffix
    outside of any group, which no group break can move to its own line. *)

val will_break : t -> bool
(** [will_break doc] checks whether [doc] contains forced line breaks. For each
    [custom_layout], it only inspects the first alternative.

    Forced breaks are not propagated through [custom_layout], because doing so
    would always select the last layout the algorithm tries. Consumers can use
    [will_break] to detect a forced break in a custom layout and explicitly add
    [break_parent] to propagate it to the parent document. *)

val to_string : width:int -> t -> string
val debug : t -> unit

val is_nil : t -> bool
