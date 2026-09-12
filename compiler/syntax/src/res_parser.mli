module Scanner = Res_scanner
module Token = Res_token
module Grammar = Res_grammar
module Reporting = Res_reporting
module Diagnostics = Res_diagnostics
module Comment = Res_comment

type region_status = Report | Silent

type cursor
type token_cache

type t = {
  filename: string;
  source: string;
  mutable cursor: cursor;
  mutable current: token_cache;
  mutable spare: token_cache;
  mutable breadcrumbs: (Grammar.t * Lexing.position) list;
  mutable errors: Reporting.parse_error list;
  mutable diagnostics: Diagnostics.t list;
  mutable comments: Comment.t list;
  mutable regions: region_status list;
  mutable warnings: (Location.t * Warnings.t) list;
}

val make : string -> string -> t

(* Queries never consume a token. At most two tokens are cached. *)
val peek : t -> Token.t
val peek2 : t -> Token.t
val start_pos : t -> Lexing.position
val end_pos : t -> Lexing.position

(* Consumed source position. Inspection and trailing trivia leave it unchanged.
   Before the first consumption, this is Lexing.dummy_pos. *)
val position : t -> Lexing.position

val expect : ?grammar:Grammar.t -> Token.t -> t -> unit
val optional : t -> Token.t -> bool
val next : t -> unit
val next_unsafe : t -> unit
val finish : t -> unit

val peek_binary_operator : t -> Token.t
val peek_slash : t -> bool
val next_template_literal_token : t -> unit
val next_regex_token : t -> unit
val lookahead : t -> (t -> 'a) -> 'a
val try_parse : t -> (t -> 'a option) -> 'a option
val err :
  ?start_pos:Lexing.position ->
  ?end_pos:Lexing.position ->
  t ->
  Diagnostics.category ->
  unit
val has_diagnostic : t -> (Diagnostics.t -> bool) -> bool
val warn : t -> Location.t -> Warnings.t -> unit
val leave_breadcrumb : t -> Grammar.t -> unit
val eat_breadcrumb : t -> unit
val begin_region : t -> unit
val end_region : t -> unit
val check_progress : position:Lexing.position -> result:'a -> t -> 'a option
