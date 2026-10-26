(* The printer emits LF line endings, but the scanner keeps a CRLF source's CRs
   inside comments, template literals and raw JS. Printed as is, those lines
   end in CR LF next to plain LF lines, and an editor or a text-mode channel
   that turns each LF into CRLF makes them CR CR LF. *)

(* Whether the first line break of [source] is CRLF *)
val uses_crlf : string -> bool

(* Replaces each CRLF with LF. A lone CR stays. *)
val to_lf : string -> string

(* Replaces each LF with CRLF. *)
val to_crlf : string -> string

(* [preserve source print] prints [source] with LF line endings and gives the
   result CRLF line endings when [source] uses them. *)
val preserve : string -> (string -> string) -> string
