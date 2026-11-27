type error = {line: int; column: int; message: string}

val check : string -> (unit, error) result
(** Checks that the text is standard JSON (RFC 8259), reporting the first
    violation, such as a comment or trailing comma, with its 1-based line and
    column (in characters). *)
