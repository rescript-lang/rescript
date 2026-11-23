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

(* The mapper returns a node as it is when all its parts come back physically
   unchanged, and rebuilds it otherwise, so a pass that rewrites little
   allocates little. The parts are mapped in the same order either way. *)

open J

let[@inline] unknown _ x = x

let[@inline] option sub self v =
  match v with
  | None -> v
  | Some x ->
    let x' = sub self x in
    if x' == x then v else Some x'

(* [Ext_list.map_sharing] maps the head before the tail, so the elements are
   mapped in list order. *)
let[@inline] list sub self x = Ext_list.map_sharing x (sub self)

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
 fun _self ({id} as m) ->
  let id' = _self.ident _self id in
  if id' == id then m else {m with id = id'}

let vident : vident fn =
 fun _self v ->
  match v with
  | Id x ->
    let x' = _self.ident _self x in
    if x' == x then v else Id x'
  | Qualified (x, name) ->
    let x' = _self.module_id _self x in
    if x' == x then v else Qualified (x', name)

let exception_ident : exception_ident fn =
 fun _self arg -> _self.ident _self arg

let for_ident : for_ident fn = fun _self arg -> _self.ident _self arg

let for_direction : for_direction fn = unknown

let object_entry : object_entry fn =
 fun _self v ->
  match v with
  | Object_property (name, x) ->
    let x' = _self.expression _self x in
    if x' == x then v else Object_property (name, x')
  | Object_spread x ->
    let x' = _self.expression _self x in
    if x' == x then v else Object_spread x'
  | Record_spread x ->
    let x' = _self.expression _self x in
    if x' == x then v else Record_spread x'

let jsx_prop : (string * expression) fn =
 fun _self ((name, value) as prop) ->
  let value' = _self.expression _self value in
  if value' == value then prop else (name, value')

let expression_desc : expression_desc fn =
 fun _self v ->
  match v with
  | Length x ->
    let x' = _self.expression _self x in
    if x' == x then v else Length x'
  | Is_null_or_undefined x ->
    let x' = _self.expression _self x in
    if x' == x then v else Is_null_or_undefined x'
  | String_append (a, b) ->
    let a' = _self.expression _self a in
    let b' = _self.expression _self b in
    if a' == a && b' == b then v else String_append (a', b')
  | Bool _ -> v
  | Typeof x ->
    let x' = _self.expression _self x in
    if x' == x then v else Typeof x'
  | Js_not x ->
    let x' = _self.expression _self x in
    if x' == x then v else Js_not x'
  | Js_bnot x ->
    let x' = _self.expression _self x in
    if x' == x then v else Js_bnot x'
  | In (a, b) ->
    let a' = _self.expression _self a in
    let b' = _self.expression _self b in
    if a' == a && b' == b then v else In (a', b')
  | Seq (a, b) ->
    let a' = _self.expression _self a in
    let b' = _self.expression _self b in
    if a' == a && b' == b then v else Seq (a', b')
  | Cond (a, b, c) ->
    let a' = _self.expression _self a in
    let b' = _self.expression _self b in
    let c' = _self.expression _self c in
    if a' == a && b' == b && c' == c then v else Cond (a', b', c')
  | Bin (op, a, b) ->
    let a' = _self.expression _self a in
    let b' = _self.expression _self b in
    if a' == a && b' == b then v else Bin (op, a', b')
  | Call (f, args, info) ->
    let f' = _self.expression _self f in
    let args' = list _self.expression _self args in
    if f' == f && args' == args then v else Call (f', args', info)
  | Tagged_template (tag, strings, values) ->
    let tag' = _self.expression _self tag in
    let values' = list _self.expression _self values in
    if tag' == tag && values' == values then v
    else Tagged_template (tag', strings, values')
  | Jsx ({callee; tag; spread; props; children; key} as jsx) ->
    let callee' = _self.expression _self callee in
    let tag' = _self.expression _self tag in
    let spread' = option _self.expression _self spread in
    let props' = list jsx_prop _self props in
    let children' = option (list _self.expression) _self children in
    let key' = option _self.expression _self key in
    if
      callee' == callee && tag' == tag && spread' == spread && props' == props
      && children' == children && key' == key
    then v
    else
      Jsx
        {
          jsx with
          callee = callee';
          tag = tag';
          spread = spread';
          props = props';
          children = children';
          key = key';
        }
  | Interpolated_template ({values} as template) ->
    let values' = list _self.expression _self values in
    if values' == values then v
    else Interpolated_template {template with values = values'}
  | Array_index (a, b) ->
    let a' = _self.expression _self a in
    let b' = _self.expression _self b in
    if a' == a && b' == b then v else Array_index (a', b')
  | Static_index (x, name, pos) ->
    let x' = _self.expression _self x in
    if x' == x then v else Static_index (x', name, pos)
  | New (f, args) ->
    let f' = _self.expression _self f in
    let args' = list _self.expression _self args in
    if f' == f && args' == args then v else New (f', args')
  | Var x ->
    let x' = _self.vident _self x in
    if x' == x then v else Var x'
  | Fun ({params; body} as fun_) ->
    let params' = list _self.ident _self params in
    let body' = _self.block _self body in
    if params' == params && body' == body then v
    else Fun {fun_ with params = params'; body = body'}
  | Str _ | Template_literal _ | Json_literal _ | Raw_js_code _ -> v
  | Array xs ->
    let xs' = list _self.expression _self xs in
    if xs' == xs then v else Array xs'
  | Optional_block (x, identity) ->
    let x' = _self.expression _self x in
    if x' == x then v else Optional_block (x', identity)
  | Caml_block (xs, mutable_flag, tag_info) ->
    let xs' = list _self.expression _self xs in
    if xs' == xs then v else Caml_block (xs', mutable_flag, tag_info)
  | Caml_block_tag (x, tag) ->
    let x' = _self.expression _self x in
    if x' == x then v else Caml_block_tag (x', tag)
  | Number _ -> v
  | Object entries ->
    let entries' = list object_entry _self entries in
    if entries' == entries then v else Object entries'
  | Undefined _ | Null -> v
  | Await x ->
    let x' = _self.expression _self x in
    if x' == x then v else Await x'
  | Spread x ->
    let x' = _self.expression _self x in
    if x' == x then v else Spread x'
  | Record_rest (fields, x) ->
    let x' = _self.expression _self x in
    if x' == x then v else Record_rest (fields, x')

let for_ident_expression : for_ident_expression fn =
 fun _self arg -> _self.expression _self arg

let finish_ident_expression : finish_ident_expression fn =
 fun _self arg -> _self.expression _self arg

let case_clause : case_clause fn =
 fun _self ({switch_body} as clause) ->
  let switch_body' = _self.block _self switch_body in
  if switch_body' == switch_body then clause
  else {clause with switch_body = switch_body'}

let string_clause : string_clause fn =
 fun _self ((tag, clause) as x) ->
  let clause' = case_clause _self clause in
  if clause' == clause then x else (tag, clause')

let int_clause : int_clause fn =
 fun _self ((tag, clause) as x) ->
  let clause' = case_clause _self clause in
  if clause' == clause then x else (tag, clause')

let catch_clause : (exception_ident * block) fn =
 fun _self ((id, handler) as x) ->
  let id' = _self.exception_ident _self id in
  let handler' = _self.block _self handler in
  if id' == id && handler' == handler then x else (id', handler')

let statement_desc : statement_desc fn =
 fun _self v ->
  match v with
  | Block b ->
    let b' = _self.block _self b in
    if b' == b then v else Block b'
  | Variable d ->
    let d' = _self.variable_declaration _self d in
    if d' == d then v else Variable d'
  | Exp x ->
    let x' = _self.expression _self x in
    if x' == x then v else Exp x'
  | If (a, b, c) ->
    let a' = _self.expression _self a in
    let b' = _self.block _self b in
    let c' = _self.block _self c in
    if a' == a && b' == b && c' == c then v else If (a', b', c')
  | While (label, a, b) ->
    let a' = _self.expression _self a in
    let b' = _self.block _self b in
    if a' == a && b' == b then v else While (label, a', b')
  | ForRange (label, init, finish, id, dir, body) ->
    let init' = option for_ident_expression _self init in
    let finish' = finish_ident_expression _self finish in
    let id' = _self.for_ident _self id in
    let dir' = for_direction _self dir in
    let body' = _self.block _self body in
    if
      init' == init && finish' == finish && id' == id && dir' == dir
      && body' == body
    then v
    else ForRange (label, init', finish', id', dir', body')
  | ForOf (label, id, a, body) ->
    let id' = _self.for_ident _self id in
    let a' = _self.expression _self a in
    let body' = _self.block _self body in
    if id' == id && a' == a && body' == body then v
    else ForOf (label, id', a', body')
  | ForAwaitOf (label, id, a, body) ->
    let id' = _self.for_ident _self id in
    let a' = _self.expression _self a in
    let body' = _self.block _self body in
    if id' == id && a' == a && body' == body then v
    else ForAwaitOf (label, id', a', body')
  | Continue _ | Break _ -> v
  | Return x ->
    let x' = _self.expression _self x in
    if x' == x then v else Return x'
  | Int_switch (e, clauses, default) ->
    let e' = _self.expression _self e in
    let clauses' = list int_clause _self clauses in
    let default' = option _self.block _self default in
    if e' == e && clauses' == clauses && default' == default then v
    else Int_switch (e', clauses', default')
  | String_switch (e, clauses, default) ->
    let e' = _self.expression _self e in
    let clauses' = list string_clause _self clauses in
    let default' = option _self.block _self default in
    if e' == e && clauses' == clauses && default' == default then v
    else String_switch (e', clauses', default')
  | Throw x ->
    let x' = _self.expression _self x in
    if x' == x then v else Throw x'
  | Try (body, catch, finally) ->
    let body' = _self.block _self body in
    let catch' = option catch_clause _self catch in
    let finally' = option _self.block _self finally in
    if body' == body && catch' == catch && finally' == finally then v
    else Try (body', catch', finally')
  | Debugger -> v

let expression : expression fn =
 fun _self ({expression_desc = desc} as e) ->
  let desc' = expression_desc _self desc in
  if desc' == desc then e else {e with expression_desc = desc'}

let statement : statement fn =
 fun _self ({statement_desc = desc} as s) ->
  let desc' = statement_desc _self desc in
  if desc' == desc then s else {s with statement_desc = desc'}

let variable_declaration : variable_declaration fn =
 fun _self ({ident; value} as d) ->
  let ident' = _self.ident _self ident in
  let value' = option _self.expression _self value in
  if ident' == ident && value' == value then d
  else {d with ident = ident'; value = value'}

let block : block fn = fun _self arg -> list _self.statement _self arg

let program : program fn =
 fun _self ({block = b} as p) ->
  let b' = _self.block _self b in
  if b' == b then p else {p with block = b'}

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
