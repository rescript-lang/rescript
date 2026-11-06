(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t = {
  lex_token: Token.t;
  lex_loc: Loc.t;
  lex_errors: (Loc.t * Parse_error.t) list;
  lex_comments: Loc.t Flow_ast.Comment.t list;
}

let with_comments result lex_comments = { result with lex_comments }

let token result = result.lex_token

let loc result = result.lex_loc

let comments result = result.lex_comments

let errors result = result.lex_errors

