module Token = Res_token
module Grammar = Res_grammar

type t
type category
type report

val get_start_pos : t -> Lexing.position [@@live] (* for playground *)
val get_end_pos : t -> Lexing.position [@@live] (* for playground *)

val explain : t -> string [@@live] (* for playground *)

val unexpected : Token.t -> (Grammar.t * Lexing.position) list -> category
val expected : ?grammar:Grammar.t -> Lexing.position -> Token.t -> category
val uident : Token.t -> category
val lident : Token.t -> category
val unclosed_string : category
val unclosed_template : category
val unclosed_comment : category
val unknown_uchar : Char.t -> category
val message : string -> category

val make : start_pos:Lexing.position -> end_pos:Lexing.position -> category -> t

val print_report :
  ?custom_intro:string option ->
  ?formatter:Format.formatter ->
  t list ->
  string ->
  unit
