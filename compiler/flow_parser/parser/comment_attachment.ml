(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Trailing comments of a node the parser has already built. Flow moves such
   comments off the inner node with an AST mapper class. ReScript never reads
   trailing comments, and creating the mapper classes was most of the
   compiler's startup work, so the nodes are returned unchanged. What is kept
   is the consumption of the comments, which decides where later comments
   attach. *)

(* The parsers that open this module use Ast for Flow_ast. *)
module Ast = Flow_ast
open Parser_env

type trailing_and_remover_result = {
  trailing: Loc.t Flow_ast.Comment.t list;
  remove_trailing: 'a. 'a -> 'a;
}

let unchanged node = node

(* Consumes the comments since the previous token: none are trailing. *)
let trailing_and_remover_after_last_loc env =
  (match Parser_env.last_loc env with
  | Some last_loc when Peek.has_eaten_comments env ->
    Parser_env.consume_comments_until env last_loc.Loc._end
  | _ -> ());
  { trailing = []; remove_trailing = unchanged }

(* Consumes and returns the comments on the same line as the previous token. *)
let trailing_and_remover_after_last_line env =
  let trailing =
    match Parser_env.last_loc env with
    | None -> []
    | Some _ when not (Peek.has_eaten_comments env) -> Eat.comments_until_next_line env
    | Some last_loc ->
      Parser_env.consume_comments_until env last_loc.Loc._end;
      Eat.comments_until_next_line env
  in
  { trailing; remove_trailing = unchanged }

let trailing_and_remover env =
  if Peek.is_line_terminator env then
    trailing_and_remover_after_last_line env
  else
    trailing_and_remover_after_last_loc env

let remove_trailing env node =
  ignore (trailing_and_remover env);
  node

let remove_trailing_opt env = function
  | None -> None
  | Some node -> Some (remove_trailing env node)

let id_remove_trailing = remove_trailing
let expression_remove_trailing = remove_trailing
let block_remove_trailing = remove_trailing
let type_params_remove_trailing = remove_trailing_opt
let type_remove_trailing = remove_trailing
let component_renders_annotation_remove_trailing = remove_trailing
let return_annotation_remove_trailing = remove_trailing
let function_params_remove_trailing = remove_trailing
let component_params_remove_trailing = remove_trailing
let component_type_params_remove_trailing = remove_trailing
let predicate_remove_trailing = remove_trailing_opt
let object_key_remove_trailing = remove_trailing
let generic_type_list_remove_trailing = remove_trailing
let class_implements_remove_trailing = remove_trailing
let string_literal_remove_trailing = remove_trailing
