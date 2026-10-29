(* The printer writes its own line breaks with the newline it is given, and
   copies comments and literals with their bytes unchanged. Formatting a file
   with the newline of its source keeps a CRLF file CRLF without touching the
   line breaks inside its string literals, which are part of the program. *)

(* The newline of [source]: CRLF when its first line break is CRLF, LF
   otherwise *)
val newline : string -> string
