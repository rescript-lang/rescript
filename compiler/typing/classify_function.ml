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

(** It seems we do the parse twice
    - in parsing
    - in code generation
*)
let classify ?(check : (Location.t * int) option) (prog : string) :
    Js_raw_info.exp =
  let prog, errors =
    let open Parser_flow in
    let env = Parser_env.init_env None prog in
    do_parse env Parse.expression false
  in
  match (check, errors) with
  | Some (loc, offset), _ :: _ ->
    Bs_flow_ast_utils.check_flow_errors ~loc ~offset errors;
    Js_exp_unknown
  | Some _, [] | None, [] -> classify_exp prog
  | None, _ :: _ -> Js_exp_unknown

let rec pattern_names acc ((_, pattern) : (_, _) Flow_ast.Pattern.t) =
  match pattern with
  | Identifier {name = _, {name}} -> name :: acc
  | Object {properties} ->
    List.fold_left
      (fun acc (property : (_, _) Flow_ast.Pattern.Object.property) ->
        match property with
        | Property (_, {pattern}) -> pattern_names acc pattern
        | RestElement (_, {argument}) -> pattern_names acc argument)
      acc properties
  | Array {elements} ->
    List.fold_left
      (fun acc (element : (_, _) Flow_ast.Pattern.Array.element) ->
        match element with
        | Element (_, {argument}) | RestElement (_, {argument}) ->
          pattern_names acc argument
        | Hole _ -> acc)
      acc elements
  | Expression _ -> acc

(* The names a toplevel statement of a raw program declares in the scope the
   program is placed in. Declarations nested in a block are not collected. *)
let rec statement_names acc ((_, statement) : (_, _) Flow_ast.Statement.t) =
  match statement with
  | FunctionDeclaration {id = Some (_, {name})}
  | ClassDeclaration {id = Some (_, {name})} ->
    name :: acc
  | VariableDeclaration {declarations} ->
    List.fold_left
      (fun acc
           ((_, {id}) :
             (_, _) Flow_ast.Statement.VariableDeclaration.Declarator.t) ->
        pattern_names acc id)
      acc declarations
  | ImportDeclaration {default; specifiers} -> (
    let acc =
      match default with
      | Some {identifier = _, {name}} -> name :: acc
      | None -> acc
    in
    match specifiers with
    | Some (ImportNamespaceSpecifier (_, (_, {name}))) -> name :: acc
    | Some (ImportNamedSpecifiers specifiers) ->
      List.fold_left
        (fun acc
             ({local; remote} :
               (_, _) Flow_ast.Statement.ImportDeclaration.named_specifier) ->
          let _, {Flow_ast.Identifier.name} =
            match local with
            | Some local -> local
            | None -> remote
          in
          name :: acc)
        acc specifiers
    | None -> acc)
  | ExportNamedDeclaration {declaration = Some declaration}
  | ExportDefaultDeclaration {declaration = Declaration declaration} ->
    statement_names acc declaration
  | _ -> acc

let classify_stmt (prog : string) : Js_raw_info.stmt =
  let result = Parser_flow.parse_program false None prog in
  match fst result with
  | _loc, {statements = []} -> Js_stmt_comment
  | _loc, {statements} ->
    Js_stmt_unknown {declared = List.fold_left statement_names [] statements}
(* we can also analayze throw
   x.x pure access
*)
