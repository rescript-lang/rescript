type t = {dependencies: string list; source: string option}
(** Published AST headers are read without deserializing the compiler's full AST.
    Cleanup needs the original source path, while graph construction needs only
    dependency names; keeping one reader prevents those wire-format rules from
    drifting. *)

val read : string -> t
