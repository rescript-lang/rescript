type mode = Jsx | Diamond

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
  mutable mode: mode list;
}

val make : filename:string -> string -> t

(* TODO: make this a record *)
val scan : t -> Lexing.position * Lexing.position * Res_token.t

val is_binary_op : string -> int -> int -> bool

val set_jsx_mode : t -> unit
val set_diamond_mode : t -> unit
val pop_mode : t -> mode -> unit

val reconsider_less_than : t -> Res_token.t

val scan_template_literal_token :
  t -> Lexing.position * Lexing.position * Res_token.t

val scan_regex : t -> Lexing.position * Lexing.position * Res_token.t
