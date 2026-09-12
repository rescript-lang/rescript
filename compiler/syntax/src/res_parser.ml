module Scanner = Res_scanner
module Diagnostics = Res_diagnostics
module Token = Res_token
module Grammar = Res_grammar
module Reporting = Res_reporting

module Comment = Res_comment

type region_status = Report | Silent

(* Two reusable slots bound lookahead without allocating a scanner per query.
   A token owns its scanner position and the effects of reading its leading
   trivia. The second token is read on a separate scanner, so inspecting it
   cannot move the first token's boundary. *)
type token_cache = {
  scanner: Scanner.t;
  (* Reuse the scanner result instead of copying its fields through three GC
     write barriers for every token. Checkpoints retain the same immutable value. *)
  mutable scanned: Lexing.position * Lexing.position * Token.t;
  mutable available: bool;
  mutable comments: Comment.t list;
  mutable diagnostics: Diagnostics.t list;
}

(* The logical cursor is the consumed source position, independent of cached
   tokens. Reuse the token end position without allocating another record. *)
type cursor = Lexing.position

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

let make_token_cache scanner end_pos =
  let cache =
    {
      scanner;
      scanned = (end_pos, end_pos, Token.Eof);
      available = false;
      comments = [];
      diagnostics = [];
    }
  in
  scanner.err <-
    (fun ~start_pos ~end_pos error ->
      cache.diagnostics <-
        Diagnostics.make ~start_pos ~end_pos error :: cache.diagnostics);
  cache

let comment_text_for_attribute cache comment =
  let txt = Comment.txt comment in
  let loc = Comment.loc comment in
  if String_literal.is_valid_utf8 txt then (loc, txt)
  else (
    cache.scanner.err ~start_pos:loc.loc_start ~end_pos:loc.loc_end
      (Diagnostics.message "Invalid code point");
    (loc, String_literal.replace_invalid_utf8 txt))

let rec read cache =
  let ((start_pos, end_pos, token) as scanned) = Scanner.scan cache.scanner in
  match token with
  | Comment c when not (Comment.is_doc_comment c || Comment.is_module_comment c)
    ->
    let _, preceding_end, _ = cache.scanned in
    Comment.set_prev_tok_end_pos c preceding_end;
    cache.comments <- c :: cache.comments;
    cache.scanned <- scanned;
    read cache
  | _ ->
    let scanned =
      match token with
      | Comment c ->
        let loc, txt = comment_text_for_attribute cache c in
        let token =
          if Comment.is_doc_comment c then Token.DocComment (loc, txt)
          else Token.ModuleComment (loc, txt)
        in
        (start_pos, end_pos, token)
      | _ -> scanned
    in
    cache.scanned <- scanned;
    cache.available <- true;
    scanned

let[@inline] current p =
  let cache = p.current in
  if cache.available then cache.scanned else read cache

let[@inline] peek p =
  let _, _, token = current p in
  token
let[@inline] start_pos p =
  let start_pos, _, _ = current p in
  start_pos
let[@inline] end_pos p =
  let _, end_pos, _ = current p in
  end_pos
let position p = p.cursor

let peek2 p =
  let _, _, token = current p in
  let cache = p.current in
  if token = Token.Eof then Token.Eof
  else
    let second = p.spare in
    if second.available then
      let _, _, token = second.scanned in
      token
    else
      let scanner = second.scanner in
      scanner.ch <- cache.scanner.ch;
      scanner.offset <- cache.scanner.offset;
      scanner.offset16 <- cache.scanner.offset16;
      scanner.line_offset <- cache.scanner.line_offset;
      scanner.lnum <- cache.scanner.lnum;
      second.scanned <- cache.scanned;
      if second.comments <> [] then second.comments <- [];
      if second.diagnostics <> [] then second.diagnostics <- [];
      let _, _, token = read second in
      token

let commit_diagnostics (p : t) (cache : token_cache) =
  if cache.diagnostics <> [] then (
    p.diagnostics <- cache.diagnostics @ p.diagnostics;
    cache.diagnostics <- [])

let has_diagnostic p predicate =
  List.exists predicate p.current.diagnostics
  || List.exists predicate p.diagnostics

let commit_effects (p : t) (cache : token_cache) =
  if cache.comments <> [] then (
    p.comments <- cache.comments @ p.comments;
    cache.comments <- []);
  commit_diagnostics p cache

(* Consume only the current token. Its successor is read by the next query. *)
let next p =
  let _, end_pos, token = current p in
  let cache = p.current in
  assert (token <> Token.Eof);
  if cache.comments <> [] || cache.diagnostics <> [] then commit_effects p cache;
  p.cursor <- end_pos;
  cache.available <- false;
  if p.spare.available then (
    p.current <- p.spare;
    p.spare <- cache)

let next_unsafe p = if peek p <> Token.Eof then next p

let err ?start_pos:from ?end_pos:until p error =
  match p.regions with
  | Report :: rest ->
    let start_pos =
      match from with
      | Some pos -> pos
      | None -> start_pos p
    in
    let end_pos =
      match until with
      | Some pos -> pos
      | None -> end_pos p
    in
    commit_diagnostics p p.current;
    p.diagnostics <- Diagnostics.make ~start_pos ~end_pos error :: p.diagnostics;
    p.regions <- Silent :: rest
  | _ -> ()

let begin_region p = p.regions <- Report :: p.regions
let end_region p =
  match p.regions with
  | [] -> ()
  | _ :: rest -> p.regions <- rest

let warn p loc warning = p.warnings <- (loc, warning) :: p.warnings

let finish p =
  if peek p = Token.Eof then commit_effects p p.current;
  List.iter
    (fun (loc, warning) -> Location.prerr_warning loc warning)
    (List.rev p.warnings);
  p.warnings <- []

let[@inline] peek_binary_operator p =
  let start_pos, _, token = current p in
  let cache = p.current in
  match token with
  | LessThan | GreaterThan ->
    let operator = Scanner.scan_binary_operator cache.scanner token in
    if operator != token then (
      p.spare.available <- false;
      cache.scanned <- (start_pos, Scanner.position cache.scanner, operator));
    operator
  | _ -> token

let peek_slash p =
  ignore (current p);
  Scanner.peek_slash p.current.scanner

let next_template_literal_token p =
  let cache = p.current in
  (* The delimiter is consumed before switching to raw text. Discard normal
     lookahead: it may already have interpreted the text as code. *)
  p.spare.available <- false;
  next_unsafe p;
  cache.scanned <- Scanner.scan_template_literal_token cache.scanner;
  cache.available <- true

let next_regex_token p =
  let start_pos, _, token = current p in
  let cache = p.current in
  let prefix_length =
    match token with
    | Forwardslash -> 1
    | ForwardslashDot -> 2
    | _ -> assert false
  in
  p.spare.available <- false;
  cache.scanned <- Scanner.scan_regex ~start_pos ~prefix_length cache.scanner

let check_progress ~position ~result p =
  if p.cursor.pos_cnum = position.Lexing.pos_cnum then None else Some result

let make source filename =
  {
    filename;
    source;
    cursor = Lexing.dummy_pos;
    current = make_token_cache (Scanner.make ~filename source) Lexing.dummy_pos;
    spare = make_token_cache (Scanner.make ~filename source) Lexing.dummy_pos;
    breadcrumbs = [];
    errors = [];
    diagnostics = [];
    comments = [];
    regions = [Report];
    warnings = [];
  }

let leave_breadcrumb p circumstance =
  p.breadcrumbs <- (circumstance, start_pos p) :: p.breadcrumbs

let eat_breadcrumb p =
  match p.breadcrumbs with
  | [] -> ()
  | _ :: crumbs -> p.breadcrumbs <- crumbs

let optional p token =
  if peek p = token then (
    next p;
    true)
  else false

let expect ?grammar token p =
  if peek p = token then next p
  else err ~start_pos:p.cursor p (Diagnostics.expected ?grammar p.cursor token)

(* Keep one checkpoint implementation for probes and parses that may commit.
   Immutable region states also roll back error suppression, including nested
   regions. Exceptions take the same restoration path as failed probes. *)
let with_checkpoint p ~commit callback =
  let scanned = current p in
  let cache = p.current in
  let spare = p.spare in
  let scanner = cache.scanner in
  let ch = scanner.ch in
  let offset = scanner.offset in
  let offset16 = scanner.offset16 in
  let line_offset = scanner.line_offset in
  let lnum = scanner.lnum in
  let available = cache.available in
  let pending_comments = cache.comments in
  let pending_diagnostics = cache.diagnostics in
  let position = p.cursor in
  let breadcrumbs = p.breadcrumbs in
  let errors = p.errors in
  let diagnostics = p.diagnostics in
  let comments = p.comments in
  let regions = p.regions in
  let warnings = p.warnings in
  let result =
    try Ok (callback p) with exn -> Error (exn, Printexc.get_raw_backtrace ())
  in
  let rollback =
    match result with
    | Ok value -> not (commit value)
    | Error _ -> true
  in
  if rollback then (
    (* Token-only probes usually leave these persistent lists unchanged.
       Avoid unnecessary GC write barriers when restoring them. *)
    scanner.ch <- ch;
    scanner.offset <- offset;
    scanner.offset16 <- offset16;
    scanner.line_offset <- line_offset;
    scanner.lnum <- lnum;
    cache.scanned <- scanned;
    cache.available <- available;
    if cache.comments != pending_comments then
      cache.comments <- pending_comments;
    if cache.diagnostics != pending_diagnostics then
      cache.diagnostics <- pending_diagnostics;
    if p.current != cache then p.current <- cache;
    if p.spare != spare then p.spare <- spare;
    spare.available <- false;
    p.cursor <- position;
    if p.breadcrumbs != breadcrumbs then p.breadcrumbs <- breadcrumbs;
    if p.errors != errors then p.errors <- errors;
    if p.diagnostics != diagnostics then p.diagnostics <- diagnostics;
    if p.comments != comments then p.comments <- comments;
    if p.regions != regions then p.regions <- regions;
    if p.warnings != warnings then p.warnings <- warnings);
  match result with
  | Ok value -> value
  | Error (exn, backtrace) -> Printexc.raise_with_backtrace exn backtrace

let lookahead p callback = with_checkpoint p ~commit:(fun _ -> false) callback
let try_parse p callback = with_checkpoint p ~commit:Option.is_some callback
