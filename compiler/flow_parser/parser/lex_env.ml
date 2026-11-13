(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Sedlexing = Flow_sedlexing

(* bol = Beginning Of Line *)
type bol = {
  line: int;
  offset: int;
}

type lex_state = { lex_errors_acc: (Loc.t * Parse_error.t) list } [@@ocaml.unboxed]

type t = {
  lex_source: File_key.t option;
  lex_lb: Sedlexing.lexbuf;
  lex_bol: bol;
  lex_in_comment_syntax: bool;
  lex_enable_comment_syntax: bool;
  lex_state: lex_state;
  lex_last_loc: Loc.t;
}

let empty_lex_state = { lex_errors_acc = [] }

(* The lex_last_loc should initially be set to the beginning of the first line, so that
   comments on the first line are reported as not being on a new line. *)
let initial_last_loc =
  { Loc.source = None; start = { Loc.line = 1; column = 0 }; _end = { Loc.line = 1; column = 0 } }

let new_lex_env lex_source lex_lb ~enable_types_in_comments =
  {
    lex_source;
    lex_lb;
    lex_bol = { line = 1; offset = 0 };
    lex_in_comment_syntax = false;
    lex_enable_comment_syntax = enable_types_in_comments;
    lex_state = empty_lex_state;
    lex_last_loc = initial_last_loc;
  }

(* copy all the mutable things so that we have a distinct lexing environment
   that does not interfere with ordinary lexer operations *)
let clone env =
  let lex_lb = Sedlexing.lexbuf_clone env.lex_lb in
  { env with lex_lb }

let set_last_loc env loc = { env with lex_last_loc = loc }

let lex_last_loc env = env.lex_last_loc

let source env = env.lex_source

let line env = env.lex_bol.line

let bol_offset env = env.lex_bol.offset

let is_in_comment_syntax env = env.lex_in_comment_syntax

let is_comment_syntax_enabled env = env.lex_enable_comment_syntax

let in_comment_syntax is_in env =
  if is_in <> env.lex_in_comment_syntax then
    { env with lex_in_comment_syntax = is_in }
  else
    env

