val get_embed_tag : string -> string option
(** [get_embed_tag name] returns [Some base] when [name] starts with
    the embed prefix "embed." and has a non-empty remainder; otherwise [None]. *)

val is_valid_embed_id : string -> bool
(** Validate embed `id`: must start with a letter and contain only
    letters, digits, and underscores. *)

val invalid_id_error_message : string
(** Centralized error message for invalid embed `id`. *)

val missing_id_error_message : string
(** Error when a config record omits `id` or provides a non-string `id`. *)

val invalid_payload_error_message : string
(** Error when embed payload is not a string literal or record literal. *)

val normalize_tag_for_symbol : string -> string
(** Convert an embed tag (validated as an attribute id) into a safe fragment
    for inclusion in a single identifier, by replacing '.' with '_'. *)
