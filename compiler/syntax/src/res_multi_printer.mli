(* Interface to print source code to res.
 * Takes a filename called "input" and returns the corresponding formatted res syntax *)
val print : ?ignore_parse_errors:bool -> string -> string [@@dead "+print"]

(* Format source code provided as a string.
 * Used by bsc -format -bs-read-stdin to format content piped from the LSP server. *)
val print_source :
  ?ignore_parse_errors:bool ->
  is_interface:bool ->
  filename:string ->
  string ->
  string
