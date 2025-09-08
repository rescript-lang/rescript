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
val double_quote : t [@@live]

(*
 * `willBreak doc` checks whether `doc` contains forced line breaks.
 * This is more or less a "workaround" to make the parent of a `customLayout` break.
 * Forced breaks are not propagated through `customLayout`; otherwise we would always
 * get the last layout the algorithm tries…
 * This might result into some weird layouts:
 *  [fn(x => {
 *     let _ = x
 *   }), fn(y => {
 *     let _ = y
 *   }), fn(z => {
 *     let _ = z
 *   })]
 *  The `[` and `]` would be a lot better broken out.
 *  Although the layout of `fn(x => {...})` is correct, we need to break its parent (the array).
 *  `willBreak` can be used in this scenario to check if the `fn…` contains any forced breaks.
 *  The consumer can then manually insert a `breakParent` doc, to manually propagate the
 *  force breaks from bottom to top.
 *)
val will_break : t -> bool

val to_string : width:int -> t -> string
val debug : t -> unit [@@live]
