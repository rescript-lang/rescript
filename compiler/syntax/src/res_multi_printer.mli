(* Interface to print source code to res.
 * Takes a filename called "input" and returns the corresponding formatted res syntax *)
val print : ?ignore_parse_errors:bool -> string -> string [@@dead "+print"]
