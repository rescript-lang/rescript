type t = {dependencies: string list; source: string option}
(** Published AST headers are read without deserializing the compiler's full AST.
    Cleanup needs the original source path, while graph construction needs only
    dependency names. The encoded dependency byte length separates those names
    from either an absolute or a relative source path and the marshalled AST. *)

val read : string -> t
