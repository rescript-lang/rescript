(* Copyright (C) 2015- Hongbo Zhang, Authors of ReScript
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

open J

let[@inline] unknown _ x = x

let[@inline] option sub self v =
  match v with
  | None -> None
  | Some v -> Some (sub self v)

let rec list sub self x =
  match x with
  | [] -> []
  | x :: xs ->
    let v = sub self x in
    v :: list sub self xs

type iter = {
  ident: ident fn;
  module_id: module_id fn;
  vident: vident fn;
  exception_ident: exception_ident fn;
  for_ident: for_ident fn;
  expression: expression fn;
  statement: statement fn;
  variable_declaration: variable_declaration fn;
  block: block fn;
  program: program fn;
}

and 'a fn = iter -> 'a -> 'a

let ident : ident fn = unknown

let module_id : module_id fn =
 fun _self {id = _x0; kind = _x1; dynamic_import = _x2} ->
  let _x0 = _self.ident _self _x0 in
  {id = _x0; kind = _x1; dynamic_import = _x2}

let required_modules : required_modules fn =
 fun _self arg -> list _self.module_id _self arg

let vident : vident fn =
 fun _self -> function
  | Id _x0 ->
    let _x0 = _self.ident _self _x0 in
    Id _x0
  | Qualified (_x0, _x1) ->
    let _x0 = _self.module_id _self _x0 in
    Qualified (_x0, _x1)

let exception_ident : exception_ident fn =
 fun _self arg -> _self.ident _self arg

let for_ident : for_ident fn = fun _self arg -> _self.ident _self arg

let for_direction : for_direction fn = unknown

let property_map : property_map fn =
 fun _self arg ->
  list
    (fun _self (_x0, _x1) ->
      let _x1 = _self.expression _self _x1 in
      (_x0, _x1))
    _self arg

let length_object : length_object fn = unknown

let expression_desc : expression_desc fn =
 fun _self -> function
  | Length (_x0, _x1) ->
    let _x0 = _self.expression _self _x0 in
    let _x1 = length_object _self _x1 in
    Length (_x0, _x1)
  | Is_null_or_undefined _x0 ->
    let _x0 = _self.expression _self _x0 in
    Is_null_or_undefined _x0
  | String_append (_x0, _x1) ->
    let _x0 = _self.expression _self _x0 in
    let _x1 = _self.expression _self _x1 in
    String_append (_x0, _x1)
  | Bool _ as v -> v
  | Typeof _x0 ->
    let _x0 = _self.expression _self _x0 in
    Typeof _x0
  | Js_not _x0 ->
    let _x0 = _self.expression _self _x0 in
    Js_not _x0
  | Js_bnot _x0 ->
    let _x0 = _self.expression _self _x0 in
    Js_bnot _x0
  | In (_x0, _x1) ->
    let _x0 = _self.expression _self _x0 in
    let _x1 = _self.expression _self _x1 in
    In (_x0, _x1)
  | Seq (_x0, _x1) ->
    let _x0 = _self.expression _self _x0 in
    let _x1 = _self.expression _self _x1 in
    Seq (_x0, _x1)
  | Cond (_x0, _x1, _x2) ->
    let _x0 = _self.expression _self _x0 in
    let _x1 = _self.expression _self _x1 in
    let _x2 = _self.expression _self _x2 in
    Cond (_x0, _x1, _x2)
  | Bin (_x0, _x1, _x2) ->
    let _x1 = _self.expression _self _x1 in
    let _x2 = _self.expression _self _x2 in
    Bin (_x0, _x1, _x2)
  | FlatCall (_x0, _x1) ->
    let _x0 = _self.expression _self _x0 in
    let _x1 = _self.expression _self _x1 in
    FlatCall (_x0, _x1)
  | Call (_x0, _x1, _x2) ->
    let _x0 = _self.expression _self _x0 in
    let _x1 = list _self.expression _self _x1 in
    Call (_x0, _x1, _x2)
  | Tagged_template (_x0, _x1, _x2) ->
    let _x0 = _self.expression _self _x0 in
    let _x1 = list _self.expression _self _x1 in
    let _x2 = list _self.expression _self _x2 in
    Tagged_template (_x0, _x1, _x2)
  | String_index (_x0, _x1) ->
    let _x0 = _self.expression _self _x0 in
    let _x1 = _self.expression _self _x1 in
    String_index (_x0, _x1)
  | Array_index (_x0, _x1) ->
    let _x0 = _self.expression _self _x0 in
    let _x1 = _self.expression _self _x1 in
    Array_index (_x0, _x1)
  | Static_index (_x0, _x1, _x2) ->
    let _x0 = _self.expression _self _x0 in
    Static_index (_x0, _x1, _x2)
  | New (_x0, _x1) ->
    let _x0 = _self.expression _self _x0 in
    let _x1 =
      option (fun _self arg -> list _self.expression _self arg) _self _x1
    in
    New (_x0, _x1)
  | Var _x0 ->
    let _x0 = _self.vident _self _x0 in
    Var _x0
  | Fun ({params; body} as fun_) ->
    let params = list _self.ident _self params in
    let body = _self.block _self body in
    Fun {fun_ with params; body}
  | Str _ as v -> v
  | Raw_js_code _ as v -> v
  | Array (_x0, _x1) ->
    let _x0 = list _self.expression _self _x0 in
    Array (_x0, _x1)
  | Optional_block (_x0, _x1) ->
    let _x0 = _self.expression _self _x0 in
    Optional_block (_x0, _x1)
  | Caml_block (_x0, _x1, _x2, _x3) ->
    let _x0 = list _self.expression _self _x0 in
    let _x2 = _self.expression _self _x2 in
    Caml_block (_x0, _x1, _x2, _x3)
  | Caml_block_tag (_x0, tag) ->
    let _x0 = _self.expression _self _x0 in
    Caml_block_tag (_x0, tag)
  | Number _ as v -> v
  | Object (_x0, _x1) ->
    let _x0 = option _self.expression _self _x0 in
    let _x1 = property_map _self _x1 in
    Object (_x0, _x1)
  | Undefined _ as v -> v
  | Null as v -> v
  | Await _x0 ->
    let _x0 = _self.expression _self _x0 in
    Await _x0
  | Spread _x0 ->
    let _x0 = _self.expression _self _x0 in
    Spread _x0

let for_ident_expression : for_ident_expression fn =
 fun _self arg -> _self.expression _self arg

let finish_ident_expression : finish_ident_expression fn =
 fun _self arg -> _self.expression _self arg

let case_clause : case_clause fn =
 fun _self {switch_body = _x0; should_break = _x1; comment = _x2} ->
  let _x0 = _self.block _self _x0 in
  {switch_body = _x0; should_break = _x1; comment = _x2}

let string_clause : string_clause fn =
 fun _self (_x0, _x1) ->
  let _x1 = case_clause _self _x1 in
  (_x0, _x1)

let int_clause : int_clause fn =
 fun _self (_x0, _x1) ->
  let _x1 = case_clause _self _x1 in
  (_x0, _x1)

let statement_desc : statement_desc fn =
 fun _self -> function
  | Block _x0 ->
    let _x0 = _self.block _self _x0 in
    Block _x0
  | Variable _x0 ->
    let _x0 = _self.variable_declaration _self _x0 in
    Variable _x0
  | Exp _x0 ->
    let _x0 = _self.expression _self _x0 in
    Exp _x0
  | If (_x0, _x1, _x2) ->
    let _x0 = _self.expression _self _x0 in
    let _x1 = _self.block _self _x1 in
    let _x2 = _self.block _self _x2 in
    If (_x0, _x1, _x2)
  | While (_x0, _x1) ->
    let _x0 = _self.expression _self _x0 in
    let _x1 = _self.block _self _x1 in
    While (_x0, _x1)
  | ForRange (_x0, _x1, _x2, _x3, _x4) ->
    let _x0 = option for_ident_expression _self _x0 in
    let _x1 = finish_ident_expression _self _x1 in
    let _x2 = _self.for_ident _self _x2 in
    let _x3 = for_direction _self _x3 in
    let _x4 = _self.block _self _x4 in
    ForRange (_x0, _x1, _x2, _x3, _x4)
  | Continue as v -> v
  | Break as v -> v
  | Return _x0 ->
    let _x0 = _self.expression _self _x0 in
    Return _x0
  | Int_switch (_x0, _x1, _x2) ->
    let _x0 = _self.expression _self _x0 in
    let _x1 = list int_clause _self _x1 in
    let _x2 = option _self.block _self _x2 in
    Int_switch (_x0, _x1, _x2)
  | String_switch (_x0, _x1, _x2) ->
    let _x0 = _self.expression _self _x0 in
    let _x1 = list string_clause _self _x1 in
    let _x2 = option _self.block _self _x2 in
    String_switch (_x0, _x1, _x2)
  | Throw _x0 ->
    let _x0 = _self.expression _self _x0 in
    Throw _x0
  | Try (_x0, _x1, _x2) ->
    let _x0 = _self.block _self _x0 in
    let _x1 =
      option
        (fun _self (_x0, _x1) ->
          let _x0 = _self.exception_ident _self _x0 in
          let _x1 = _self.block _self _x1 in
          (_x0, _x1))
        _self _x1
    in
    let _x2 = option _self.block _self _x2 in
    Try (_x0, _x1, _x2)
  | Debugger as v -> v

let expression : expression fn =
 fun _self {expression_desc = _x0; comment = _x1} ->
  let _x0 = expression_desc _self _x0 in
  {expression_desc = _x0; comment = _x1}

let statement : statement fn =
 fun _self {statement_desc = _x0; comment = _x1} ->
  let _x0 = statement_desc _self _x0 in
  {statement_desc = _x0; comment = _x1}

let variable_declaration : variable_declaration fn =
 fun _self {ident = _x0; value = _x1; property = _x2; ident_info = _x3} ->
  let _x0 = _self.ident _self _x0 in
  let _x1 = option _self.expression _self _x1 in
  {ident = _x0; value = _x1; property = _x2; ident_info = _x3}

let block : block fn = fun _self arg -> list _self.statement _self arg

let program : program fn =
 fun _self {block = _x0; exports = _x1; export_set = _x2} ->
  let _x0 = _self.block _self _x0 in
  {block = _x0; exports = _x1; export_set = _x2}

let deps_program : deps_program fn =
 fun _self {program = _x0; modules = _x1; side_effect = _x2} ->
  let _x0 = _self.program _self _x0 in
  let _x1 = required_modules _self _x1 in
  {program = _x0; modules = _x1; side_effect = _x2}

let super : iter =
  {
    ident;
    module_id;
    vident;
    exception_ident;
    for_ident;
    expression;
    statement;
    variable_declaration;
    block;
    program;
  }
