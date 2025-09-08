module Scanner = Res_scanner
module Diagnostics = Res_diagnostics
module Token = Res_token
module Grammar = Res_grammar
module Reporting = Res_reporting

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

let err ?start_pos ?end_pos p error =
  match p.regions with
  | ({contents = Report} as region) :: _ ->
    let d =
      Diagnostics.make
        ~start_pos:
          (match start_pos with
          | Some pos -> pos
          | None -> p.start_pos)
        ~end_pos:
          (match end_pos with
          | Some pos -> pos
          | None -> p.end_pos)
        error
    in
    p.diagnostics <- d :: p.diagnostics;
    region := Silent
  | _ -> ()

let begin_region p = p.regions <- ref Report :: p.regions
let end_region p =
  match p.regions with
  | [] -> ()
  | _ :: rest -> p.regions <- rest

let doc_comment_to_attribute_token comment =
  let txt = Comment.txt comment in
  let loc = Comment.loc comment in
  Token.DocComment (loc, txt)

let module_comment_to_attribute_token comment =
  let txt = Comment.txt comment in
  let loc = Comment.loc comment in
  Token.ModuleComment (loc, txt)

(* Advance to the next non-comment token and store any encountered comment
 * in the parser's state. Every comment contains the end position of its
 * previous token to facilite comment interleaving *)
let rec next ?prev_end_pos p =
  if p.token = Eof then assert false;
  let prev_end_pos =
    match prev_end_pos with
    | Some pos -> pos
    | None -> p.end_pos
  in
  let start_pos, end_pos, token = Scanner.scan p.scanner in
  match token with
  | Comment c ->
    if Comment.is_doc_comment c then (
      p.token <- doc_comment_to_attribute_token c;
      p.prev_end_pos <- prev_end_pos;
      p.start_pos <- start_pos;
      p.end_pos <- end_pos)
    else if Comment.is_module_comment c then (
      p.token <- module_comment_to_attribute_token c;
      p.prev_end_pos <- prev_end_pos;
      p.start_pos <- start_pos;
      p.end_pos <- end_pos)
    else (
      Comment.set_prev_tok_end_pos c p.end_pos;
      p.comments <- c :: p.comments;
      p.prev_end_pos <- p.end_pos;
      p.end_pos <- end_pos;
      next ~prev_end_pos p)
  | _ ->
    p.token <- token;
    p.prev_end_pos <- prev_end_pos;
    p.start_pos <- start_pos;
    p.end_pos <- end_pos

let next_unsafe p = if p.token <> Eof then next p

let next_template_literal_token p =
  let start_pos, end_pos, token =
    Scanner.scan_template_literal_token p.scanner
  in
  p.token <- token;
  p.prev_end_pos <- p.end_pos;
  p.start_pos <- start_pos;
  p.end_pos <- end_pos

let next_regex_token p =
  let start_pos, end_pos, token = Scanner.scan_regex p.scanner in
  p.token <- token;
  p.prev_end_pos <- p.end_pos;
  p.start_pos <- start_pos;
  p.end_pos <- end_pos

let check_progress ~prev_end_pos ~result p =
  if p.end_pos == prev_end_pos then None else Some result

let make ?(mode = ParseForTypeChecker) src filename =
  let scanner = Scanner.make ~filename src in
  let parser_state =
    {
      mode;
      scanner;
      token = Token.Semicolon;
      start_pos = Lexing.dummy_pos;
      prev_end_pos = Lexing.dummy_pos;
      end_pos = Lexing.dummy_pos;
      breadcrumbs = [];
      errors = [];
      diagnostics = [];
      comments = [];
      regions = [ref Report];
    }
  in
  parser_state.scanner.err <-
    (fun ~start_pos ~end_pos error ->
      let diagnostic = Diagnostics.make ~start_pos ~end_pos error in
      parser_state.diagnostics <- diagnostic :: parser_state.diagnostics);
  next parser_state;
  parser_state

let leave_breadcrumb p circumstance =
  let crumb = (circumstance, p.start_pos) in
  p.breadcrumbs <- crumb :: p.breadcrumbs

let eat_breadcrumb p =
  match p.breadcrumbs with
  | [] -> ()
  | _ :: crumbs -> p.breadcrumbs <- crumbs

let optional p token =
  if p.token = token then
    let () = next p in
    true
  else false

let expect ?grammar token p =
  if p.token = token then next p
  else
    let error = Diagnostics.expected ?grammar p.prev_end_pos token in
    err ~start_pos:p.prev_end_pos p error

(* Don't use immutable copies here, it trashes certain heuristics
 * in the ocaml compiler, resulting in massive slowdowns of the parser *)
let lookahead p callback =
  let err = p.scanner.err in
  let ch = p.scanner.ch in
  let offset = p.scanner.offset in
  let offset16 = p.scanner.offset16 in
  let line_offset = p.scanner.line_offset in
  let lnum = p.scanner.lnum in
  let mode = p.scanner.mode in
  let token = p.token in
  let start_pos = p.start_pos in
  let end_pos = p.end_pos in
  let prev_end_pos = p.prev_end_pos in
  let breadcrumbs = p.breadcrumbs in
  let errors = p.errors in
  let diagnostics = p.diagnostics in
  let comments = p.comments in

  let res = callback p in

  p.scanner.err <- err;
  p.scanner.ch <- ch;
  p.scanner.offset <- offset;
  p.scanner.offset16 <- offset16;
  p.scanner.line_offset <- line_offset;
  p.scanner.lnum <- lnum;
  p.scanner.mode <- mode;
  p.token <- token;
  p.start_pos <- start_pos;
  p.end_pos <- end_pos;
  p.prev_end_pos <- prev_end_pos;
  p.breadcrumbs <- breadcrumbs;
  p.errors <- errors;
  p.diagnostics <- diagnostics;
  p.comments <- comments;

  res
