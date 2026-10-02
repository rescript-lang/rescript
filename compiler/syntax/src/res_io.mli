(* utilities to read and write to/from files or stdin *)

(* reads the contents of "filename" into a string *)
val read_file : filename:string -> string

(* writes "content" into file with name "filename" *)
val write_file : filename:string -> contents:string -> unit
