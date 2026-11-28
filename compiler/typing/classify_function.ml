(* Copyright (C) 2020- Hongbo Zhang, Authors of ReScript
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

let rec is_obj_literal (x : _ Flow_ast.Expression.t) : bool =
  match snd x with
  | Identifier (_, {name = "undefined"})
  | StringLiteral _ | BooleanLiteral _ | NullLiteral _ | NumberLiteral _
  | BigIntLiteral _ | RegExpLiteral _ | ModuleRefLiteral _ ->
    true
  | Unary {operator = Minus; argument} -> is_obj_literal argument
  | Object {properties} -> List.for_all is_literal_kv properties
  | Array {elements} ->
    List.for_all
      (fun x ->
        match x with
        | Flow_ast.Expression.Array.Expression x -> is_obj_literal x
        | _ -> false)
      elements
  | _ -> false

and is_literal_kv (x : _ Flow_ast.Expression.Object.property) =
  match x with
  | Property (_, Init {value}) -> is_obj_literal value
  | _ -> false

let classify_exp (prog : _ Flow_ast.Expression.t) : Js_raw_info.exp =
  match prog with
  | ( _,
      Function
        {
          id = _;
          params = _, {params};
          async = false;
          generator = false;
          predicate = None;
        } ) ->
    Js_function {arity = List.length params; arrow = false}
  | ( _,
      ArrowFunction
        {
          id = None;
          params = _, {params};
          async = false;
          generator = false;
          predicate = None;
        } ) ->
    Js_function {arity = List.length params; arrow = true}
  | _, StringLiteral {comments}
  | _, BooleanLiteral {comments}
  | _, NullLiteral comments
  | _, NumberLiteral {comments}
  | _, BigIntLiteral {comments}
  | _, RegExpLiteral {comments}
  | _, ModuleRefLiteral {comments} ->
    let comment =
      match comments with
      | None -> None
      | Some {leading = [(_, {kind = Block; text = comment})]} ->
        Some ("/*" ^ comment ^ "*/")
      | Some {leading = [(_, {kind = Line; text = comment})]} ->
        Some ("//" ^ comment)
      | Some _ -> None
    in
    Js_literal {comment}
  | _, Identifier (_, {name = "undefined"}) -> Js_literal {comment = None}
  | _, _ ->
    if is_obj_literal prog then Js_literal {comment = None} else Js_exp_unknown
  | exception _ -> Js_exp_unknown

(* Parses [code] as an expression. Besides the tree and the errors, it gives
   whether a line comment runs to the end of [code]: whether the last comment
   the lexer recorded is a line comment that starts after the last token, or
   in code without a token. A "//" in a string, a template or a regular
   expression is not a comment, and a line comment ends at any line
   terminator. Code that the parser rejects counts as ending in one. *)
let parse_exp (code : string) =
  let open Parser_flow in
  let env = Parser_env.init_env None code in
  let prog, errors = do_parse env Parse.expression false in
  let last_token = Parser_env.last_loc env in
  (* The comments before the end of the input reach [Parser_env.comments]
     when the end is eaten *)
  Parser_env.Eat.token env;
  let ends_in_line_comment =
    errors <> []
    ||
    match (Parser_env.comments env, last_token) with
    | (loc, {Flow_ast.Comment.kind = Line}) :: _, Some last ->
      Loc.pos_cmp loc.start last._end >= 0
    | (_, {Flow_ast.Comment.kind = Line}) :: _, None -> true
    | _ -> false
  in
  (prog, errors, ends_in_line_comment)

let classify ?(check : (Location.t * int) option) (prog : string) :
    Js_raw_info.exp =
  let prog, errors, _ = parse_exp prog in
  match (check, errors) with
  | Some (loc, offset), _ :: _ ->
    Bs_flow_ast_utils.check_flow_errors ~loc ~offset errors;
    Js_exp_unknown
  | Some _, [] | None, [] -> classify_exp prog
  | None, _ :: _ -> Js_exp_unknown

let classify_raw (code : string) : Js_raw_info.t =
  let prog, errors, ends_in_line_comment = parse_exp code in
  let kind =
    match errors with
    | [] -> classify_exp prog
    | _ :: _ -> Js_exp_unknown
  in
  {code; code_info = Exp kind; ends_in_line_comment}

let classify_stmt (prog : string) : Js_raw_info.stmt =
  let result = Parser_flow.parse_program false None prog in
  match fst result with
  | _loc, {statements = []} -> Js_stmt_comment
  | _ -> Js_stmt_unknown
(* we can also analayze throw
   x.x pure access
*)
