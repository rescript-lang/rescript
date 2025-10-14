val get_embed_tag : string -> string option
(** [get_embed_tag name] returns [Some base] when [name] starts with
    the embed prefix "embed." and has a non-empty remainder; otherwise [None]. *)
