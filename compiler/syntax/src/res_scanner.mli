type char_encoding

type t = {
  filename: string;
  src: string;
  mutable err:
    start_pos:Lexing.position ->
    end_pos:Lexing.position ->
    Res_diagnostics.category ->
    unit;
  mutable ch: char_encoding; (* current character *)
  mutable offset: int; (* current byte offset *)
  mutable offset16: int;
      (* current number of utf16 code units since line start *)
  mutable line_offset: int; (* current line offset *)
  mutable lnum: int; (* current line number *)
}

val make : filename:string -> string -> t
val position : t -> Lexing.position

(* TODO: make this a record *)
val scan : t -> Lexing.position * Lexing.position * Res_token.t

val is_binary_op : string -> int -> int -> bool

(* Extend a just-scanned < or > with an adjacent operator suffix. This never
   skips trivia, and returns the original token without advancing if there is
   no suffix. *)
val scan_binary_operator : t -> Res_token.t -> Res_token.t

val scan_template_literal_token :
  t -> Lexing.position * Lexing.position * Res_token.t

val scan_regex :
  start_pos:Lexing.position ->
  prefix_length:int ->
  t ->
  Lexing.position * Lexing.position * Res_token.t

(* Look ahead to see if the next non-whitespace character is a slash *)
val peek_slash : t -> bool
