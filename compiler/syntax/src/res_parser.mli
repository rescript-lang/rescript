module Scanner = Res_scanner
module Token = Res_token
module Grammar = Res_grammar
module Reporting = Res_reporting
module Diagnostics = Res_diagnostics
module Comment = Res_comment

type mode = ParseForTypeChecker | Default

type region_status = Report | Silent

type t = {
  mode: mode;
  mutable scanner: Scanner.t;
  mutable token: Token.t;
  mutable start_pos: Lexing.position;
  mutable end_pos: Lexing.position;
  mutable prev_end_pos: Lexing.position;
  mutable breadcrumbs: (Grammar.t * Lexing.position) list;
  mutable errors: Reporting.parse_error list;
  mutable diagnostics: Diagnostics.t list;
  mutable comments: Comment.t list;
  mutable regions: region_status ref list;
}

val make : ?mode:mode -> string -> string -> t

val expect : ?grammar:Grammar.t -> Token.t -> t -> unit
val optional : t -> Token.t -> bool
val next : ?prev_end_pos:Lexing.position -> t -> unit
val next_unsafe : t -> unit (* Does not assert on Eof, makes no progress *)
val next_template_literal_token : t -> unit
val next_regex_token : t -> unit
val lookahead : t -> (t -> 'a) -> 'a
val err :
  ?start_pos:Lexing.position ->
  ?end_pos:Lexing.position ->
  t ->
  Diagnostics.category ->
  unit

val leave_breadcrumb : t -> Grammar.t -> unit
val eat_breadcrumb : t -> unit

val begin_region : t -> unit
val end_region : t -> unit

val check_progress : prev_end_pos:Lexing.position -> result:'a -> t -> 'a option
