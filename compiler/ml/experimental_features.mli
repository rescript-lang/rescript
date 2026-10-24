type feature =
  | LetUnwrap
  | MultilineItemSpacing
      (** The formatter separates module items that span several lines with a
          blank line. *)

val enable_from_string : string -> unit

val is_enabled : feature -> bool
val to_string : feature -> string
val reset : unit -> unit
